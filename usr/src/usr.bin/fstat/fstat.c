/*
 * Copyright (c) 1987 Regents of the University of California.
 * All rights reserved.  The Berkeley software License Agreement
 * specifies the terms and conditions for redistribution.
 */

#ifndef lint
char copyright[] =
"@(#) Copyright (c) 1987 Regents of the University of California.\n\
 All rights reserved.\n";
#endif not lint

#ifndef lint
static char sccsid[] = "@(#)fstat.c	5.2 (Berkeley) %G%";
#endif not lint

/*
 *  fstat 
 */
#include <stdio.h>
#include <ctype.h>
#include <nlist.h>
#include <pwd.h>
#include <sys/param.h>
#include <sys/ioctl.h>
#include <sys/tty.h>
#include <sys/dir.h>
#include <sys/user.h>
#include <sys/proc.h>
#include <machine/pte.h> 
#include <sys/vm.h>
#include <sys/text.h>
#include <sys/stat.h>
#include <math.h>
#include <sys/vlimit.h>
#include <sys/inode.h>
#include <sys/socket.h>
#include <sys/socketvar.h>
#include <sys/domain.h>
#include <sys/protosw.h>
#include <net/route.h>
#include <netinet/in.h>
#include <netinet/in_pcb.h>
#include <sys/unpcb.h>
#define KERNEL
#include <sys/file.h>
#undef KERNEL

#ifdef ULTRIX	
   		/* UFS -> GFS */
#    define	inode	gnode
#    define	x_iptr	x_gptr
#    define	i_dev	g_dev
#    define	i_number g_number
#    define	i_mode	g_mode
#    define	i_size	g_size
#endif ULTRIX

char *emalloc();
char *getinetproto();

#define vprintf	if (vflg) printf
#define WD	-1
#define TEXT	-2

int	pcbpf, nswap, kmem, mem, swap, uid, pid;
int	uflg, fflg, inum, Mdev, mdev, special, vflg, nproc, pflg;
int sflg, kflg; /*4.2*/
int argaddr; /*4.2*/

#define clear(x) 	((int)x & 0x7fffffff)

struct pte *Sysmap = 0;

struct nlist nl[] = {
	{ "_proc" },
#define	X_PROC		0
	{ "_Usrptmap" },
#define	X_USRPTMA	1
	{ "_usrpt" },
#define	X_USRPT		2
	{ "_nswap" },
#define	X_NSWAP		3
	{ "_nproc" },
#define	X_NPROC		4
	{ "_Sysmap" },
#define SSYSMAP		5
	{ "_Syssize" },
#define SSYSSIZE	6
	{ "" },
};

char 	*stypename[] = {
	"unused",	/* 0 */
	"stream", 	/* 1 */
	"dgram",	/* 2 */
	"raw",		/* 3 */
	"rdm",		/* 4 */
	"seqpak"	/* 5 */
};
#define STYPEMAX 5

char	*uname;


struct	proc proc[8], *mproc;		/* 8 = a few, for less syscalls */
struct	pte *Usrptma, *usrpt;

int	paduser1;		/* avoid hardware mem clobbering botch */
union {
	struct	user user;
	char	upages[UPAGES][NBPG];
} user;
int	paduser2;		/* avoid hardware mem clobbering botch */
#define u	user.user

char	*kmemf, *memf, *swapf, *nlistf;

extern int	errno;
char	stdoutBuf[BUFSIZ];

main(argc, argv)
char **argv;
{
	register int i, j;
	off_t procp;
	dev_t	dev;

	argv++;
	while (--argc > 0) {
		if (strcmp(*argv, "-v") == 0) {
			vflg++;
			argv++;
			continue;
		} 
		if (strcmp(*argv, "-u") == 0) {
			if (uflg++)
				usage();
			if ((uid = getuname(*(++argv))) < 0) {
				fprintf(stderr, "%s: unknown user\n", *argv);
				exit(1);
			}
			--argc;
			argv++;
			continue;
		} 
		if (strcmp(*argv, "-f") == 0) {
			if (fflg++)
				usage();
			if ((dev = getfname(*(++argv))) < 0) {
				perror(*argv);
				exit(1);
			}
			--argc;
			argv++;
			continue;
		}

		if (strcmp(*argv, "-p") == 0) {
			if (pflg++ || ((pid = Atoi(*(++argv))) <= 0)) {
				usage();
				perror(*argv);
				exit(1);
			}
			--argc;
			argv++;
			continue;
		}

		/* admit missing -u, -f, -p */
		/* it's an expert system! */
		if ((pid = Atoi(*argv)) > 0) {
			if (pflg++)
				usage();
			continue;
		}
		if (fflg && uflg)
			usage();
		if (uflg) {
			/* it must be a file */
			fflg++;
			if ((dev = getfname(*argv)) < 0) {
				perror(*argv);
				exit(1);
			}
			argv++;
			continue;
		}
		if (fflg) {
			/* it must be a user */
			uflg++;
			if ((uid = getuname(*argv)) < 0) {
				fprintf(stderr,
					"%s: unknown user\n", *argv);
				exit(1);
			}
			argv++;
			continue;
		}
		/* !uflg && !fflg -- which is it? */
		if ((dev = getfname(*argv)) >= 0)
			fflg++;		/* could be a file */
		if ((uid = getuname(*argv)) >= 0)
			uflg++;		/* could be a user */
		if ((!uflg ^ !fflg) == 0)
			usage();	/* could be either/neither */
		argv++;
		continue;
	}

	if (fflg) {
		Mdev = major(dev);
		mdev = minor(dev);
	}

	if (chdir("/dev") < 0) {
		perror("/dev");
		exit(1);
	}

	printf("USER\t CMD\t      PID    FD\tDEVICE\tINODE\t  SIZE\tTYPE\n");
	openfiles();
	getkvars();
	procp = getw((off_t) nl[X_PROC].n_value);
	nproc = getw((off_t) nl[X_NPROC].n_value);
	for (i=0; i<nproc; i += 8) {
		lseek(kmem, (long) procp, 0);
		j = nproc - i;
		if (j > 8)
			j = 8;
		j *= sizeof (struct proc);
		if (read(kmem, (char *) proc, j) != j)
			cantread("proc table", kmemf);
		procp += j;
		for (j = j / sizeof (struct proc) - 1; j >= 0; j--) {
			mproc = &proc[j];
			if (mproc->p_stat == 0)
				continue;
			doproc();
		}
	}
	exit(0);
}

long
getw(loc)
	off_t loc;
{
	long word;

	lseek(kmem, (long) loc, 0);
	if (read(kmem, (char *) &word, sizeof (word)) != sizeof (word))
		vprintf("error reading kmem at %x\n", loc);
	return (word);
}

openfiles()
{

	kmemf = "kmem";
	kmem = open(kmemf, 0);
	if (kmem < 0) {
		perror(kmemf);
		exit(1);
	}
	memf = "mem";
	mem = open(memf, 0);
	if (mem < 0) {
		perror(memf);
		exit(1);
	}
	swapf = "drum";
	swap = open(swapf, 0);
	if (swap < 0) {
		perror(swapf);
		exit(1);
	}
}

getkvars()
{
	nlistf = "/vmunix";
	nlist(nlistf, nl);
	if (nl[0].n_type == 0) {
		fprintf(stderr, "%s: No namelist\n", nlistf);
		exit(1);
	}
	Usrptma = (struct pte *) nl[X_USRPTMA].n_value;
	usrpt = (struct pte *) nl[X_USRPT].n_value;
	lseek(kmem, (long) nl[X_NSWAP].n_value, 0);
	if (read(kmem, (char *) &nswap, sizeof (nswap)) != sizeof (nswap)) {
		cantread("nswap", kmemf);
		exit(1);
	}
}

cantread(what, fromwhat)
	char *what, *fromwhat;
{

	vprintf("fstat: error reading %s from %s", what, fromwhat);
}

doproc()
{
	struct passwd	*getpwuid();


	if (uflg && mproc->p_uid != uid)
		return;
	if (pflg && mproc->p_pid != pid)
		return;
	if (mproc->p_stat != SZOMB && getu() == 0)
		return;
	uname = getpwuid(mproc->p_uid)->pw_name;
	dotext();
	getf();
}

getu()
{
	struct pte *pteaddr, apte;
	struct pte arguutl[UPAGES+CLSIZE];
	register int i;
	int ncl, size;

	size = sizeof (struct user);
	if ((mproc->p_flag & SLOAD) == 0) {
		if (swap < 0)
			return (0);
		(void) lseek(swap, (long)dtob(mproc->p_swaddr), 0);
		if (read(swap, (char *)&user.user, size) != size) {
			fprintf(stderr, "ps: cant read u for pid %d from %s\n",
			    mproc->p_pid, swapf);
			return (0);
		}
		pcbpf = 0;
		argaddr = 0;
		return (1);
	}
	pteaddr = &Usrptma[btokmx(mproc->p_p0br) + mproc->p_szpt - 1];
	klseek(kmem, (long)pteaddr, 0);
	if (read(kmem, (char *)&apte, sizeof(apte)) != sizeof(apte)) {
		printf("fstat: cant read indir pte to get u for pid %d from %s\n",
		    mproc->p_pid, swapf);
		return (0);
	}
	klseek(mem,
	    (long)ctob(apte.pg_pfnum+1) - (UPAGES+CLSIZE) * sizeof (struct pte),
		0);
	if (read(mem, (char *)arguutl, sizeof(arguutl)) != sizeof(arguutl)) {
		printf("fstat: cant read page table for u of pid %d from %s\n",
		    mproc->p_pid, kmemf);
		return (0);
	}
	if (arguutl[0].pg_fod == 0 && arguutl[0].pg_pfnum)
		argaddr = ctob(arguutl[0].pg_pfnum);
	else
		argaddr = 0;
	pcbpf = arguutl[CLSIZE].pg_pfnum;
	ncl = (size + NBPG*CLSIZE - 1) / (NBPG*CLSIZE);
	while (--ncl >= 0) {
		i = ncl * CLSIZE;
		klseek(mem, (long)ctob(arguutl[CLSIZE+i].pg_pfnum), 0);
		if (read(mem, user.upages[i], CLSIZE*NBPG) != CLSIZE*NBPG) {
			printf("fstat: cant read page %d of u of pid %d from %s\n",
			    arguutl[CLSIZE+i].pg_pfnum, mproc->p_pid, memf);
			return(0);
		}
	}
	return (1);
}

#define	NMAX	8
#define	NUID	2048

dotext()
{
	struct text	text;

	lseek(kmem, (long) mproc->p_textp, 0);
	if (read(kmem, (char *) &text, sizeof(text)) != sizeof(text)) {
		cantread("text table", kmemf);
		return;
	}
	if (text.x_flag == 0)
		return;
	itrans(DTYPE_INODE, text.x_iptr, TEXT);
}

char 	*itypename[] = {
	"unk",
#define UNK 0	/* unknown */
	"chr",
#define CHR 1	
	"dir",
#define DIR 2
	"blk",
#define BLK 3
	"reg",
#define REG 4
	"lnk",
#define LNK 5
	"soc"
#define SOC 6
};

itype(mode)
{
	switch(mode&IFMT) {
	case IFCHR:
		return(CHR);
	case IFDIR:
		return(DIR);
	case IFBLK:
		return(BLK);
	case IFREG:
		return(REG);
	case IFLNK:
		return(LNK);
	case IFSOCK:
		return(SOC);
	default:
		return(0);
	}
}

itrans(ftype, g, fno)
struct inode	*g;  /* if ftype is inode */
{
	struct inode	inode;
	dev_t	idev;
	int 	type;
	char 	*comm;

	if (g == 0 && ! fflg) {
		goto skip;
	}
	lseek(kmem, (long) g, 0);
	if (read(kmem, (char *) &inode, sizeof(inode))
					!= sizeof(inode)) {
		vprintf("error %d reading inode at %x from kmem\n", errno, g);
		return;
	}
	if (special)
		idev = inode.i_dev;
	else
		idev = inode.i_dev;
	if (fflg && major(idev) != Mdev)
		return;	
	if (fflg && minor(idev) != mdev)
		return;	
	if (inum && inode.i_number != inum)
		return;
skip:
	if (mproc->p_pid == 0)
		comm = "swapper";
	else if (mproc->p_pid == 2)
		comm = "pagedaemon";
	else
		comm = u.u_comm;
	printf("%-8.8s %-10.10s %5d  ", uname, comm, mproc->p_pid);
	if (fno == WD)
		printf("  wd");
	else if (fno == TEXT)
		printf("text");
	else
		printf("%4d", fno);

	if (g == 0) {
		printf("* (deallocated)\n");
		return;
	}

	if (ftype == DTYPE_INODE) {
		type = itype(inode.i_mode);  /* determine inode type */
		printf("\t%2d, %2d\t%5d\t%6d\t%3s\n", major(inode.i_dev), minor(inode.i_dev),
			inode.i_number, type == SOC?0:inode.i_size, itypename[type]);
	}
	else if (ftype == DTYPE_SOCKET) {
		socktrans((struct socket *)g);
	}
#ifdef DTYPE_PORT
	else if (ftype == DTYPE_PORT) {
		printf("* (fifo / named pipe)\n");
	}
#endif DTYPE_PORT
	else {
		printf("* (unknown file type)\n");
	}
}

socktrans(sock)
struct socket *sock;
{
	struct socket 	so;
	struct protosw 	proto;
	struct domain 	dom;
	char	dname[32];	/* domain name, e.g. "inet" */
	char 	c;
	char 	*cp;
	int	i;
	char 	*stype;
	struct inpcb 	inpcb;
	struct unpcb 	unpcb;

	/* fill in socket */
	lseek(kmem, (long) sock, 0);
	if (read(kmem, (char *) &so, sizeof(struct socket)) 
	    != sizeof(struct socket)){
		vprintf("error %d reading socket at %x from kmem\n", errno, sock);
		return;
	}

	/* fill in protosw entry */
	lseek(kmem, (long) so.so_proto, 0);
	if (read(kmem, (char *) &proto, sizeof(struct protosw)) 
	    != sizeof(struct protosw)){
		vprintf("error %d reading protosw at %x from kmem\n", errno, so.so_proto);
		return;
	}

	/* fill in domain */
	lseek(kmem, (long) proto.pr_domain, 0);
	if (read(kmem, (char *) &dom, sizeof(struct domain)) 
	    != sizeof(struct domain)){
		vprintf("error %d reading domain at %x from kmem\n", errno, proto.pr_domain);
		return;
	}

	/* Grab domain name */
	lseek(kmem, (long) dom.dom_name, 0);
	for (cp=dname, i=0; i < 30; i++, cp++) { /* 30 leaves room for null byte */
		if (read(kmem, (char *)&c, sizeof(char)) != sizeof(char)) {
		    vprintf("error %d reading char at %x from kmem\n", errno, dom.dom_name+i);
		    break;
		}
		if (c == '\0')
			break;
		*cp = c;
	}
	*cp='\0';
	/* kludge "internet" --> "inet" for brevity */
	if (dom.dom_family == AF_INET)	
		strcpy(dname, "inet");

	if (so.so_type < 1 || so.so_type > STYPEMAX)
		stype = (char *)sprintf(emalloc(10),"unk%d", so.so_type);
	else
		stype = stypename[so.so_type];

	/* print sock type, sock state, and domain name */
	printf("* (%s %s %x", dname, stype, so.so_state);

	/* 
	 * protocol specific formating 
	 *
	 * Try to find interesting things to print.  For tcp, the
	 * interesting thing is the address of the tcpcb, for udp
	 * and others, just the inpcb (socket pcb).  For unix
	 * domain, its the address of the socket pcb and the address of
	 * the connected pcb (if connected).  Otherwise just print
	 * the protocol number and address of the socket itself. The
	 * idea is not to duplicate netstat, but to make available
	 * enough information for further analysis. 
	 */
	if (dom.dom_family == AF_INET) {
		/* print name of protocol number */
		printf(" %s", getinetproto(proto.pr_protocol));
		if (proto.pr_protocol == IPPROTO_TCP ) {
			if (so.so_pcb) {
				lseek(kmem, (long) so.so_pcb, 0);
				if (read(kmem, (char *) &inpcb, sizeof(struct inpcb)) 
				    != sizeof(struct inpcb)){
					vprintf("error %d reading inpcb at %x from kmem\n",
					     errno, so.so_pcb);
					return;
				}
				printf(" %x", inpcb.inp_ppcb);
			}
		} else if (so.so_pcb) {
				printf(" %x", so.so_pcb);
		}
	} else if (dom.dom_family == AF_UNIX) {	
		/* print address of pcb and connected pcb */
		if (so.so_pcb) {
			printf(" %x", so.so_pcb);
			lseek(kmem, (long) so.so_pcb, 0);
			if (read(kmem, (char *) &unpcb, sizeof(struct unpcb)) 
			    != sizeof(struct unpcb)){
				vprintf("error %d reading unpcb at %x from kmem\n",
				     errno, so.so_pcb);
				return;
			}
			if (unpcb.unp_conn)
				printf(" -> %x", unpcb.unp_conn);
		}
	} else {
		/* print protocol number and socket address */
		printf(" %d %x", proto.pr_protocol, sock);
	}
	printf(")\n");
}

char *
getinetproto(number)
{
	switch(number) {
	case 0:	 return("ip");
	case 1:	 return("icmp");
	case 2:	 return("ggp");
	case 6:	 return("tcp");
	case 8:	 return("egp");
	case 12: return("pup");
	case 17: return("udp");
	case 22: return("idp");
	case 255: return("raw");
	default: return((char *)sprintf(emalloc(16),"%d",number));
	}
}
		
char *
emalloc(size)
{
	char *cp;
	cp = (char *)malloc(size);
	if (cp < 0) {
		fprintf(stderr,"Out of space.\n");
		exit(1);
	}
	return(cp);
}

struct  file  *
getf()
{
	int	i;
	struct file	file;

	itrans(DTYPE_INODE, u.u_cdir, WD);
	for (i = 0; i < NOFILE; i++) {
		if (u.u_ofile[i] == 0)
			continue;
		lseek(kmem, (long) u.u_ofile[i], 0);
		if (read(kmem, (char *) &file, sizeof(file)) != sizeof(file)) {
			cantread("file", kmemf);
			continue;
		}
		/*printf("flag: %x count: %x ",file.f_flag, file.f_count);
		/*fflush(stdout);
		 */
		itrans(file.f_type, file.f_data, i); 
	}
}

usage()
{
	fputs("usage: fstat [-u user] [-f filename] [-p pid]\n", stderr);
	exit(1);
}

getuname(uname)
char	*uname;
{
	struct passwd	*passwd, *getpwnam();
	
	if ((passwd = getpwnam(uname)) == NULL)
		return(-1);
	return(passwd->pw_uid);
}

getfname(filename)
char	*filename;
{
	struct	stat statbuf;

	if (stat(filename, &statbuf) != 0)
		return(-1);

	/*
	 *	if file is block special, look for open files on it
	 */
	if ((statbuf.st_mode & S_IFMT) != S_IFBLK) {
		inum = statbuf.st_ino;
		return(statbuf.st_dev);
	} else {
		special++;
		inum = 0;
		return(statbuf.st_rdev);
	}
}

Atoi(p)
register char *p;
{
	register int n = 0;

	while(*p >= '0' && *p <= '9')
		n = n*10 + *p++ - '0';
	return(*p ? -n : n);
}

klseek(fd, loc, off)
	int fd;
	long loc;
	int off;
{
	static int	sizeSysmap;

	if( kflg && Sysmap == 0)
		{/* initialize Sysmap */

		sizeSysmap = nl[SSYSSIZE].n_value * sizeof( struct pte);
		Sysmap = (struct pte *)calloc( sizeSysmap, 1);
		lseek( kmem, clear( nl[SSYSMAP].n_value), 0);
		if( read( kmem, Sysmap, sizeSysmap) != sizeSysmap)
			{
			printf( "Cant read system page table\n");
			exit(1);
			}
		}
	if( kflg && (loc&0x80000000))
		{/* do mapping for kernel virtual addresses */
		struct pte *ptep;

		loc &= 0x7fffffff;
		ptep = &Sysmap[btop(loc)];
		if( (char *)ptep - (char *)Sysmap > sizeSysmap)
			{
			printf( "no system pte for %s\n", loc);
			exit(1);
			}
		if( ptep->pg_v == 0)
			{
			printf( "system pte invalid for %x\n", loc);
			exit(1);
			}
		loc = (off_t)((loc&PGOFSET) + ptob(ptep->pg_pfnum));
		}
	(void) lseek(fd, (long)loc, off);
}

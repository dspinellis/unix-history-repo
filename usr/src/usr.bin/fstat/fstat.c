/*
 * Copyright (c) 1987 Regents of the University of California.
 * All rights reserved.  The Berkeley software License Agreement
 * specifies the terms and conditions for redistribution.
 */

#ifndef lint
char copyright[] =
"@(#) Copyright (c) 1987 Regents of the University of California.\n\
 All rights reserved.\n";
#endif /* !lint */

#ifndef lint
static char sccsid[] = "@(#)fstat.c	5.5 (Berkeley) %G%";
#endif /* !lint */

/*
 *  fstat 
 */
#include <machine/pte.h> 

#include <sys/param.h>
#include <sys/ioctl.h>
#include <sys/tty.h>
#include <sys/dir.h>
#include <sys/user.h>
#include <sys/proc.h>
#include <sys/vm.h>
#include <sys/text.h>
#include <sys/stat.h>
#include <sys/vlimit.h>
#include <sys/inode.h>
#include <sys/socket.h>
#include <sys/socketvar.h>
#include <sys/domain.h>
#include <sys/protosw.h>
#include <sys/unpcb.h>
#define KERNEL
#include <sys/file.h>
#undef KERNEL
#include <net/route.h>
#include <netinet/in.h>
#include <netinet/in_pcb.h>
#include <stdio.h>
#include <math.h>
#include <nlist.h>
#include <pwd.h>

#ifdef	ULTRIX
		/* UFS -> GFS */
#    define	inode	gnode
#    define	x_iptr	x_gptr
#    define	i_dev	g_dev
#    define	i_number g_number
#    define	i_mode	g_mode
#    define	i_size	g_size
#endif	/* ULTRIX */

#define	TEXT	-2
#define	WD	-1
#define	vprintf	if (vflg) printf

struct devs {
	struct devs	*next;
	dev_t	dev;
	int	inum;
} devs;

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
	{ "" },
};

struct	proc proc[8], *mproc;		/* 8 = a few, for less syscalls */
struct	pte *Usrptma, *usrpt;
union {
	struct	user user;
	char	upages[UPAGES][NBPG];
} user;

static int	nproc, nswap, kmem, mem, swap, uid, pid;
static int	uflg, fflg, vflg, pflg;
static char	*kmemf = "kmem",
		*memf = "mem",
		*swapf = "drum",
		*nlistf = "/vmunix",
		*uname;

extern int	errno;

off_t	lseek();
long	lgetw();
char	*emalloc(), *getinetproto();

main(argc, argv)
	int	argc;
	char	**argv;
{
	extern char	*optarg;
	extern int	optind;
	register int i, j;
	off_t procp;
	int	ch;

	if (chdir("/dev") < 0) {
		perror("/dev");
		exit(1);
	}

	while ((ch = getopt(argc, argv, "p:u:v")) != EOF)
		switch((char)ch) {
		case 'p':
			if (pflg++ || ((pid = atoi(optarg)) <= 0)) {
				usage();
				exit(1);
			}
			break;
		case 'u':
			if (uflg++)
				usage();
			if ((uid = getuname(optarg)) < 0) {
				fprintf(stderr, "%s: unknown user\n", optarg);
				exit(1);
			}
			break;
		case 'v':
			vflg++;
			break;
		case '?':
		default:
			usage();
		}

	for (argv += optind; *argv; ++argv) {
		fflg = 1;
		getfname(*argv);
	}

	printf("USER\t CMD\t      PID    FD\tDEVICE\tINODE\t  SIZE\tTYPE\n");
	openfiles();
	getkvars();
	procp = lgetw((off_t)nl[X_PROC].n_value);
	nproc = (int)lgetw((off_t)nl[X_NPROC].n_value);
	for (i = 0; i < nproc; i += 8) {
		(void)lseek(kmem, (off_t)procp, 0);
		j = MIN(nproc - i, 8);
		j *= sizeof(struct proc);
		if (read(kmem, (char *)proc, j) != j)
			cantread("proc table", kmemf);
		procp += j;
		for (j = j / sizeof(struct proc) - 1; j >= 0; j--) {
			mproc = &proc[j];
			if (mproc->p_stat == 0)
				continue;
			doproc();
		}
	}
	exit(0);
}

static long
lgetw(loc)
	off_t loc;
{
	long word;

	(void)lseek(kmem, (off_t)loc, L_SET);
	if (read(kmem, (char *) &word, sizeof(word)) != sizeof(word))
		vprintf("error reading kmem at %lx\n", loc);
	return(word);
}

static
openfiles()
{
	kmem = open(kmemf, O_RDONLY, 0);
	if (kmem < 0) {
		perror(kmemf);
		exit(1);
	}
	mem = open(memf, O_RDONLY, 0);
	if (mem < 0) {
		perror(memf);
		exit(1);
	}
	swap = open(swapf, O_RDONLY, 0);
	if (swap < 0) {
		perror(swapf);
		exit(1);
	}
}

static
getkvars()
{
	if (nlist(nlistf, nl) == -1 || !nl[0].n_type) {
		fprintf(stderr, "%s: No namelist\n", nlistf);
		exit(1);
	}
	Usrptma = (struct pte *)nl[X_USRPTMA].n_value;
	usrpt = (struct pte *)nl[X_USRPT].n_value;
	(void)lseek(kmem, (off_t)nl[X_NSWAP].n_value, L_SET);
	if (read(kmem, (char *)&nswap, sizeof(nswap)) != sizeof(nswap)) {
		cantread("nswap", kmemf);
		exit(1);
	}
}

static
cantread(what, fromwhat)
	char *what, *fromwhat;
{
	vprintf("fstat: error reading %s from %s", what, fromwhat);
}

static
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

static
getu()
{
	struct pte *pteaddr, apte;
	struct pte arguutl[UPAGES+CLSIZE];
	register int i;
	int ncl, size;

	size = sizeof(struct user);
	if ((mproc->p_flag & SLOAD) == 0) {
		if (swap < 0)
			return (0);
		(void)lseek(swap, (off_t)dtob(mproc->p_swaddr), L_SET);
		if (read(swap, (char *)&user.user, size) != size) {
			fprintf(stderr, "fstat: cant read u for pid %d from %s\n",
			    mproc->p_pid, swapf);
			return(0);
		}
		return(1);
	}
	pteaddr = &Usrptma[btokmx(mproc->p_p0br) + mproc->p_szpt - 1];
	(void)lseek(kmem, (off_t)pteaddr, L_SET);
	if (read(kmem, (char *)&apte, sizeof(apte)) != sizeof(apte)) {
		printf("fstat: cant read indir pte to get u for pid %d from %s\n",
		    mproc->p_pid, swapf);
		return(0);
	}
	(void)lseek(mem, (off_t)ctob(apte.pg_pfnum+1) - (UPAGES+CLSIZE)
	    * sizeof(struct pte), L_SET);
	if (read(mem, (char *)arguutl, sizeof(arguutl)) != sizeof(arguutl)) {
		printf("fstat: cant read page table for u of pid %d from %s\n",
		    mproc->p_pid, kmemf);
		return(0);
	}
	ncl = (size + NBPG*CLSIZE - 1) / (NBPG*CLSIZE);
	while (--ncl >= 0) {
		i = ncl * CLSIZE;
		(void)lseek(mem, (off_t)ctob(arguutl[CLSIZE+i].pg_pfnum), L_SET);
		if (read(mem, user.upages[i], CLSIZE*NBPG) != CLSIZE*NBPG) {
			printf("fstat: cant read page %u of u of pid %d from %s\n",
			    arguutl[CLSIZE+i].pg_pfnum, mproc->p_pid, memf);
			return(0);
		}
	}
	return(1);
}

static
dotext()
{
	struct text	text;

	(void)lseek(kmem, (off_t)mproc->p_textp, L_SET);
	if (read(kmem, (char *) &text, sizeof(text)) != sizeof(text)) {
		cantread("text table", kmemf);
		return;
	}
	if (text.x_flag == 0)
		return;
	itrans(DTYPE_INODE, text.x_iptr, TEXT);
}

char	*itypename[] = {
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

static
itype(mode)
	u_short mode;
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

static
itrans(ftype, g, fno)
	int ftype, fno;
	struct inode	*g;		/* if ftype is inode */
{
	struct inode	inode;
	dev_t	idev;
	int	type;
	char	*comm;

	if (g || fflg) {
		(void)lseek(kmem, (off_t)g, L_SET);
		if (read(kmem, (char *)&inode, sizeof(inode)) != sizeof(inode)) {
			vprintf("error %d reading inode at %x from kmem\n", errno, (int)g);
			return;
		}
		idev = inode.i_dev;
		if (fflg && !devmatch(idev, inode.i_number))
			return;
	}
	if (mproc->p_pid == 0)
		comm = "swapper";
	else if (mproc->p_pid == 2)
		comm = "pagedaemon";
	else
		comm = user.user.u_comm;
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

	switch(ftype) {
	case DTYPE_INODE:
		type = itype(inode.i_mode);	/* determine inode type */
		printf("\t%2d, %2d\t%5lu\t%6ld\t%3s\n", major(inode.i_dev),
		    minor(inode.i_dev), inode.i_number,
		    type == SOC ? 0 : inode.i_size, itypename[type]);
		break;
	case DTYPE_SOCKET:
		socktrans((struct socket *)g);
		break;
#ifdef DTYPE_PORT
	case DTYPE_PORT:
		printf("* (fifo / named pipe)\n");
		break;
#endif
	default:
		printf("* (unknown file type)\n");
	}
}

static
devmatch(idev, inum)
	dev_t idev;
	ino_t inum;
{
	struct devs *d = &devs;

	for (d = d->next; d; d = d->next)
		if (d->dev == idev && (!d->inum || d->inum == inum))
			return(1);
	return(0);
}

static
socktrans(sock)
	struct socket *sock;
{
	static char	*stypename[] = {
		"unused",	/* 0 */
		"stream", 	/* 1 */
		"dgram",	/* 2 */
		"raw",		/* 3 */
		"rdm",		/* 4 */
		"seqpak"	/* 5 */
	};
#define	STYPEMAX 5
	struct socket	so;
	struct protosw	proto;
	struct domain	dom;
	struct inpcb	inpcb;
	struct unpcb	unpcb;
	int	i;
	char	c, *cp, *stype, *strcpy(), *strcat();
	char	dname[32];	/* domain name, e.g. "inet" */

	/* fill in socket */
	(void)lseek(kmem, (off_t)sock, L_SET);
	if (read(kmem, (char *)&so, sizeof(struct socket))
	    != sizeof(struct socket)) {
		vprintf("error %d reading socket at %x from kmem\n", errno, (int)sock);
		return;
	}

	/* fill in protosw entry */
	(void)lseek(kmem, (off_t)so.so_proto, L_SET);
	if (read(kmem, (char *)&proto, sizeof(struct protosw))
	    != sizeof(struct protosw)) {
		vprintf("error %d reading protosw at %x from kmem\n", errno, (int)so.so_proto);
		return;
	}

	/* fill in domain */
	(void)lseek(kmem, (off_t)proto.pr_domain, L_SET);
	if (read(kmem, (char *)&dom, sizeof(struct domain))
	    != sizeof(struct domain)) {
		vprintf("error %d reading domain at %x from kmem\n", errno, (int)proto.pr_domain);
		return;
	}

	/* Grab domain name */
	(void)lseek(kmem, (off_t)dom.dom_name, L_SET);
	/* 30 leaves room for null byte */
	for (cp = dname, i = 0; i < 30; i++, cp++) {
		if (read(kmem, (char *)&c, sizeof(char)) != sizeof(char)) {
		    vprintf("error %d reading char at %x from kmem\n", errno, (int)(dom.dom_name+i));
		    break;
		}
		if (c == '\0')
			break;
		*cp = c;
	}
	*cp = '\0';
	/* kludge "internet" --> "inet" for brevity */
	if (dom.dom_family == AF_INET)
		(void)strcpy(dname, "inet");

	if (so.so_type < 1 || so.so_type > STYPEMAX) {
		stype = emalloc(10);
		(void)sprintf(stype, "unk%d", so.so_type);
	}
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
				(void)lseek(kmem, (off_t)so.so_pcb, L_SET);
				if (read(kmem, (char *)&inpcb, sizeof(struct inpcb))
				    != sizeof(struct inpcb)){
					vprintf("error %d reading inpcb at %x from kmem\n",
					     errno, (int)so.so_pcb);
					return;
				}
				printf(" %x", (int)inpcb.inp_ppcb);
			}
		}
		else if (so.so_pcb)
			printf(" %x", (int)so.so_pcb);
	}
	else if (dom.dom_family == AF_UNIX) {
		/* print address of pcb and connected pcb */
		if (so.so_pcb) {
			printf(" %x", (int)so.so_pcb);
			(void)lseek(kmem, (off_t)so.so_pcb, L_SET);
			if (read(kmem, (char *)&unpcb, sizeof(struct unpcb))
			    != sizeof(struct unpcb)){
				vprintf("error %d reading unpcb at %x from kmem\n",
				     errno, (int)so.so_pcb);
				return;
			}
			if (unpcb.unp_conn) {
				char shoconn[4]; *shoconn = 0;

				if (!(so.so_state & SS_CANTRCVMORE))
					(void)strcat(shoconn, "<");
				(void)strcat(shoconn, "-");
				if (!(so.so_state & SS_CANTSENDMORE))
					(void)strcat(shoconn, ">");
				printf(" %s %x", shoconn, (int)unpcb.unp_conn);
			}
		}
	}
	else		/* print protocol number and socket address */
		printf(" %d %x", proto.pr_protocol, (int)sock);
	printf(")\n");
}

static char *
getinetproto(number)
	int number;
{
	char *cp;

	switch(number) {
	case 0:	  return("ip");
	case 1:	  return("icmp");
	case 2:	  return("ggp");
	case 6:	  return("tcp");
	case 8:	  return("egp");
	case 12:  return("pup");
	case 17:  return("udp");
	case 22:  return("idp");
	case 255: return("raw");
	default:
		(void)sprintf(cp = emalloc(16), "%d", number);
		return(cp);
	}
}

static char *
emalloc(size)
	int size;
{
	char *cp, *malloc();

	if (!(cp = (char *)malloc((u_int)size))) {
		fprintf(stderr, "fstat: out of space.\n");
		exit(1);
	}
	return(cp);
}

static struct file *
getf()
{
	int i;
	struct file lfile;

	itrans(DTYPE_INODE, user.user.u_cdir, WD);
	for (i = 0; i < NOFILE; i++) {
		if (user.user.u_ofile[i] == 0)
			continue;
		(void)lseek(kmem, (off_t)user.user.u_ofile[i], L_SET);
		if (read(kmem, (char *)&lfile, sizeof(lfile))
		    != sizeof(lfile)) {
			cantread("file", kmemf);
			continue;
		}
		itrans(lfile.f_type, (struct inode *)lfile.f_data, i);
	}
}

static
usage()
{
	fputs("usage: fstat [-v] [-u user] [-p pid] [filename ...]\n", stderr);
	exit(1);
}

static
getuname(arg_uname)
	char *arg_uname;
{
	struct passwd *passwd, *getpwnam();

	return((passwd = getpwnam(arg_uname)) ? passwd->pw_uid : -1);
}

static
getfname(filename)
	char *filename;
{
	struct	stat statbuf;
	struct devs *d, *oldd;
	dev_t dev;
	int inum;

	if (stat(filename, &statbuf)) {
		perror(filename);
		exit(1);
	}

	/*
	 * if file is block special, look for open files on it
	 */
	if ((statbuf.st_mode & S_IFMT) != S_IFBLK) {
		inum = statbuf.st_ino;
		dev = statbuf.st_dev;
	}
	else {
		inum = 0;
		dev = statbuf.st_rdev;
	}
	for (d = oldd = &devs; d; oldd = d, d = d->next)
		;
	d = (struct devs *)emalloc(sizeof(struct devs));
	oldd->next = d;
	d->next = NULL;
	d->dev = dev;
	d->inum = inum;
}

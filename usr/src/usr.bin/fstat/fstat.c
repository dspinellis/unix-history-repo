/*
 * Copyright (c) 1987 Regents of the University of California.
 * All rights reserved.
 *
 * Redistribution and use in source and binary forms are permitted
 * provided that the above copyright notice and this paragraph are
 * duplicated in all such forms and that any documentation,
 * advertising materials, and other materials related to such
 * distribution and use acknowledge that the software was developed
 * by the University of California, Berkeley.  The name of the
 * University may not be used to endorse or promote products derived
 * from this software without specific prior written permission.
 * THIS SOFTWARE IS PROVIDED ``AS IS'' AND WITHOUT ANY EXPRESS OR
 * IMPLIED WARRANTIES, INCLUDING, WITHOUT LIMITATION, THE IMPLIED
 * WARRANTIES OF MERCHANTIBILITY AND FITNESS FOR A PARTICULAR PURPOSE.
 */

#ifndef lint
char copyright[] =
"@(#) Copyright (c) 1987 Regents of the University of California.\n\
 All rights reserved.\n";
#endif /* not lint */

#ifndef lint
char sccsid[] = "@(#)fstat.c	5.22 (Berkeley) %G%";
#endif /* not lint */

/*
 *  fstat 
 */
#include <machine/pte.h>

#include <sys/param.h>
#include <sys/user.h>
#include <sys/proc.h>
#include <sys/text.h>
#include <sys/stat.h>
#include <sys/time.h>
#include <sys/vnode.h>
#include <sys/socket.h>
#include <sys/socketvar.h>
#include <sys/domain.h>
#include <sys/protosw.h>
#include <sys/unpcb.h>
#include <sys/vmmac.h>
#define	KERNEL
#define NFS
#include <sys/file.h>
#include <sys/mount.h>
#include <ufs/inode.h>
#include <nfs/nfsv2.h>
#include <nfs/nfs.h>
#include <nfs/nfsnode.h>
#undef KERNEL

#include <net/route.h>
#include <netinet/in.h>
#include <netinet/in_systm.h>
#include <netinet/ip.h>
#include <netinet/in_pcb.h>

#include <kvm.h>
#include <paths.h>
#include <ctype.h>
#include <nlist.h>
#include <pwd.h>
#include <strings.h>
#include <stdio.h>

#define	TEXT	-1
#define	CDIR	-2
#define	RDIR	-3
#define	TRACE	-4

typedef struct devs {
	struct	devs *next;
	dev_t	dev;
	ino_t	ino;
	char	*name;
} DEVS;
DEVS *devs;

struct  filestat {
	long	fsid;
	long	fileid;
	u_long	size;
	dev_t	rdev;
};

struct nlist nl[] = {
	{ "" },
};

int 	fsflg,	/* show files on same filesystem as file(s) argument */
	pflg,	/* show files open by a particular pid */
	uflg;	/* show files open by a particular (effective) user */
int 	checkfile; /* true if restricting to particular files or filesystems */
int	nflg;	/* (numerical) display f.s. and rdev as dev_t */
int	vflg;	/* display errors in locating kernel data objects etc... */

#define dprintf	if (vflg) fprintf

extern int errno;
off_t lseek();

main(argc, argv)
	int argc;
	char **argv;
{
	register struct passwd *passwd;
	int what = KINFO_PROC_ALL, arg = 0;
	struct passwd *getpwnam(), *getpwuid();
	struct proc *p;
	extern char *optarg;
	extern int optind;
	int ch, size;
	char *malloc();


	while ((ch = getopt(argc, argv, "p:u:fnv")) != EOF)
		switch((char)ch) {
		case 'p':
			if (pflg++)
				usage();
			if (!isdigit(*optarg)) {
				fputs("fstat: -p option requires a process id.\n", stderr);
				usage();
			}
			what = KINFO_PROC_PID;
			arg = atoi(optarg);
			break;
		case 'u':
			if (uflg++)
				usage();
			if (!(passwd = getpwnam(optarg))) {
				(void)fprintf(stderr, "%s: unknown uid\n",
				    optarg);
				exit(1);
			}
			what = KINFO_PROC_UID;
			arg = passwd->pw_uid;
			break;
		case 'f':
			fsflg++;
			break;
		case 'n':
			nflg++;
			break;
		case 'v':
			vflg++;
			break;
		case '?':
		default:
			usage();
		}

	if (*(argv += optind)) {
		for (; *argv; ++argv) {
			if (getfname(*argv))
				checkfile = 1;
		}
		if (!checkfile)	/* file(s) specified, but none accessable */
			exit(1);
	}
	if (fsflg && !checkfile) {	
		/* -f with no files means use wd */
		if (getfname(".") == 0)
			exit(1);
		checkfile = 1;
	}

	if (kvm_openfiles(NULL, NULL, NULL) == -1) {
		fprintf(stderr, "fstat: %s\n", kvm_geterr());
		exit(1);
	}
	if (kvm_nlist(nl) != 0) {
		(void)fprintf(stderr, "%s: no namelist: %s\n", kvm_geterr());
		exit(1);
	}
	if (kvm_getprocs(what, arg) == -1) {
		fprintf(stderr, "fstat: %s\n", kvm_geterr());
		exit(1);
	}
	if (nflg)
fputs("USER     CMD        PID   FD FST  DEV    INUM TYPE SZ|DV", stdout);
	else
fputs("USER     CMD        PID   FD FST MNT        INUM TYPE SZ|DV", stdout);
	if (checkfile && fsflg == 0)
		fputs(" NAME\n", stdout);	
	else
		putchar('\n');

	while ((p = kvm_nextproc()) != NULL) {
		if (p->p_stat == SZOMB)
			continue;
		dofiles(p);
	}
	exit(0);
}

char	*Uname, *Comm;
int	Pid;

#define PREFIX(i) printf("%-8.8s %-8.8s %5d", Uname, Comm, Pid); \
	switch(i) { \
	case TEXT: \
		fputs(" text", stdout); \
		break; \
	case CDIR: \
		fputs("   wd", stdout); \
		break; \
	case RDIR: \
		fputs(" root", stdout); \
		break; \
	case TRACE: \
		fputs("   tr", stdout); \
		break; \
	default: \
		printf(" %4d", i); \
		break; \
	}

/*
 * print open files attributed to this process
 */
dofiles(p)
	struct proc *p;
{
	int i;
	char *user_from_uid();
	struct file file;
	struct user *up = kvm_getu(p);
	struct vnode *xvptr;

	Uname = user_from_uid(p->p_uid, 0);
	Pid = p->p_pid;
	Comm = p->p_comm;

	if (up == NULL) {
		dprintf(stderr, "can't read u for pid %d\n", Pid);
		return;
	}
	/*
	 * root directory vnode, if one
	 */
	if (up->u_rdir)
		vtrans(up->u_rdir, RDIR);
	/*
	 * text vnode
	 */
	if (p->p_textp && 
	    kvm_read((int)p->p_textp+(int)&((struct text *)0)->x_vptr, &xvptr,
	    sizeof (struct vnode *)) == sizeof (struct vnode *) &&
	    xvptr != NULL)
		vtrans(xvptr, TEXT);
	/*
	 * current working directory vnode
	 */
	vtrans(up->u_cdir, CDIR);
	/*
	 * ktrace vnode, if one
	 */
	if (p->p_tracep)
		vtrans(p->p_tracep, TRACE);
	/*
	 * open files
	 */
	for (i = 0; i <= up->u_lastfile; i++) {
		if (up->u_ofile[i] == 0)
			continue;
		if (kvm_read(up->u_ofile[i], &file, sizeof (struct file)) !=
		    sizeof (struct file)) {
			dprintf(stderr, "can't read file %d for pid %d\n",
				i, Pid);
			continue;
		}
		if (file.f_type == DTYPE_VNODE)
			vtrans((struct vnode *)file.f_data, i);
		else if (file.f_type == DTYPE_SOCKET && checkfile == 0)
			socktrans((struct socket *)file.f_data, i);
		else {
			dprintf(stderr, 
				"unknown file type %d for file %d of pid %d\n",
				file.f_type, i, Pid);
		}
	}
}

vtrans(vp, i)
	struct vnode *vp;
{
	struct vnode vn;
	struct filestat fst;
	char *filename = NULL;
	char *fstype;
	char *getmnton(), *vtype();
	int nodata = 0;

	if (kvm_read((off_t)vp, &vn, sizeof (struct vnode)) != 
	    sizeof (struct vnode)) {
		dprintf(stderr, "can't read vnode at %x for pid %d\n",
			vp, Pid);
		return;
	}
	switch (vn.v_tag) {
	case VT_NON:
		fstype = " non";
		nodata = 1;
		break;
	case VT_UFS:
		fstype = " ufs";
		ufs_filestat(&vn, &fst);
		break;
	case VT_MFS:
		fstype = " mfs";
		ufs_filestat(&vn, &fst);
		break;
	case VT_NFS:
		fstype = " nfs";
		nfs_filestat(&vn, &fst);
		break;
	default: {
		static char unknown[10];
		sprintf(fstype = unknown, " ?%d", vn.v_tag);
		nodata = 1;
		break;;
	}
	}
	if (checkfile) {
		int fsmatch = 0;
		register DEVS *d;

		if (nodata)
			return;
		for (d = devs; d != NULL; d = d->next)
			if (d->dev == fst.fsid) {
				fsmatch = 1;
				if (d->ino == fst.fileid) {
					filename = d->name;
					break;
				}
			}
		if (fsmatch == 0 || (filename == NULL && fsflg == 0))
			return;
	}
	PREFIX(i);
	fputs(fstype, stdout);
	if (nodata) {
		printf(" -       -    -    -\n");
		return;
	}
	if (nflg)
		printf(" %2d,%-2d", major(fst.fsid), minor(fst.fsid));
	else
		printf(" %-8s", getmnton(vn.v_mount));
	printf(" %6d %3s", fst.fileid, vtype(vn.v_type));
	switch (vn.v_type) {
	case VBLK:
		if (nflg)
			printf("  %2d,%-2d", major(fst.rdev), minor(fst.rdev));
		else
			printf(" %-6s", devname(fst.rdev, 0));
		break;
	case VCHR:
		if (nflg)
			printf("  %2d/%-2d", major(fst.rdev), minor(fst.rdev));
		else
			printf(" %-6s", devname(fst.rdev, 1));
		break;
	default:
		printf(" %6d", fst.size);
	}
	if (filename && !fsflg)
		printf(" %s", filename);
		
	putchar('\n');
}

ufs_filestat(vp, fsp)
	struct vnode *vp;
	struct filestat *fsp;
{
	struct inode *ip = VTOI(vp);

	fsp->fsid = (long)ip->i_dev;
	fsp->fileid = (long)ip->i_number;
	fsp->size = (u_long)ip->i_size;
	fsp->rdev = ip->i_rdev;
}

nfs_filestat(vp, fsp)
	struct vnode *vp;
	struct filestat *fsp;
{
	struct nfsnode *np = VTONFS(vp);

	fsp->fsid = np->n_vattr.va_fsid;
	fsp->fileid = np->n_vattr.va_fileid;
	fsp->size = np->n_vattr.va_size;
	fsp->rdev = np->n_vattr.va_rdev;
}


char *
getmnton(m)
	struct mount *m;
{
	static struct mount mount;
	static struct mtab {
		struct mtab *next;
		struct mount *m;
		char mntonname[MNAMELEN];
	} *mhead = NULL;
	register struct mtab *mt;

	for (mt = mhead; mt != NULL; mt = mt->next)
		if (m == mt->m)
			return (mt->mntonname);
	if (kvm_read((off_t)m, &mount, sizeof(struct mount)) != 
	    sizeof(struct mount)) {
		fprintf(stderr, "can't read mount table at %x\n", m);
		return (NULL);
	}
	if ((mt = (struct mtab *)malloc(sizeof (struct mtab))) == NULL) {
		fprintf(stderr, "out of memory\n");
		exit(1);
	}
	mt->m = m;
	bcopy(&mount.m_stat.f_mntonname[0], &mt->mntonname[0], MNAMELEN);
	mt->next = mhead;
	mhead = mt;
	return (mt->mntonname);
}

char *
vtype(type)
	enum vtype type;
{

	switch(type) {
	case VNON:
		return("non");
	case VREG:
		return("reg");
	case VDIR:
		return("dir");
	case VBLK:
		return("blk");
	case VCHR:
		return("chr");
	case VLNK:
		return("lnk");
	case VSOCK:
		return("soc");
	case VFIFO:
		return("fif");
	case VBAD:
		return("bad");
	default: {
		static char unknown[10];
		sprintf(unknown, "?%d", type);
		return(unknown);
	}
	}
	/*NOTREACHED*/
}

socktrans(sock, i)
	struct socket *sock;
{
	static char *stypename[] = {
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
	int len;
	char dname[32], *strcpy();

	PREFIX(i);

	/* fill in socket */
	if (kvm_read((off_t)sock, (char *)&so, sizeof(struct socket))
	    != sizeof(struct socket)) {
		dprintf(stderr, "can't read sock at %x\n", sock);
		return;
	}

	/* fill in protosw entry */
	if (kvm_read((off_t)so.so_proto, (char *)&proto, sizeof(struct protosw))
	    != sizeof(struct protosw)) {
		dprintf(stderr, "can't read protosw at %x", so.so_proto);
		return;
	}

	/* fill in domain */
	if (kvm_read((off_t)proto.pr_domain, (char *)&dom, sizeof(struct domain))
	    != sizeof(struct domain)) {
		dprintf(stderr, "can't read domain at %x\n", proto.pr_domain);
		return;
	}

	/*
	 * grab domain name
	 * kludge "internet" --> "inet" for brevity
	 */
	if (dom.dom_family == AF_INET)
		(void)strcpy(dname, "inet");
	else {
		if ((len = kvm_read((off_t)dom.dom_name, dname, sizeof(dname) - 1)) < 0) {
			dprintf(stderr, "can't read domain name at %x\n",
				dom.dom_name);
			dname[0] = '\0';
		}
		else
			dname[len] = '\0';
	}

	if ((u_short)so.so_type > STYPEMAX)
		(void)printf("* %s ?%d", dname, so.so_type);
	else
		(void)printf("* %s %s", dname, stypename[so.so_type]);

	/* 
	 * protocol specific formatting
	 *
	 * Try to find interesting things to print.  For tcp, the interesting
	 * thing is the address of the tcpcb, for udp and others, just the
	 * inpcb (socket pcb).  For unix domain, its the address of the socket
	 * pcb and the address of the connected pcb (if connected).  Otherwise
	 * just print the protocol number and address of the socket itself.
	 * The idea is not to duplicate netstat, but to make available enough
	 * information for further analysis.
	 */
	switch(dom.dom_family) {
	case AF_INET:
		getinetproto(proto.pr_protocol);
		if (proto.pr_protocol == IPPROTO_TCP ) {
			if (so.so_pcb) {
				if (kvm_read((off_t)so.so_pcb, (char *)&inpcb, sizeof(struct inpcb))
				    != sizeof(struct inpcb)){
					dprintf(stderr, 
					     "can't read inpcb at %x\n", so.so_pcb);
					return;
				}
				(void)printf(" %x", (int)inpcb.inp_ppcb);
			}
		}
		else if (so.so_pcb)
			(void)printf(" %x", (int)so.so_pcb);
		break;
	case AF_UNIX:
		/* print address of pcb and connected pcb */
		if (so.so_pcb) {
			(void)printf(" %x", (int)so.so_pcb);
			if (kvm_read((off_t)so.so_pcb, (char *)&unpcb, sizeof(struct unpcb))
			    != sizeof(struct unpcb)){
				dprintf(stderr, "can't read unpcb at %x\n",
					so.so_pcb);
				return;
			}
			if (unpcb.unp_conn) {
				char shoconn[4], *cp;

				cp = shoconn;
				if (!(so.so_state & SS_CANTRCVMORE))
					*cp++ = '<';
				*cp++ = '-';
				if (!(so.so_state & SS_CANTSENDMORE))
					*cp++ = '>';
				*cp = '\0';
				(void)printf(" %s %x", shoconn,
				    (int)unpcb.unp_conn);
			}
		}
		break;
	default:
		/* print protocol number and socket address */
		(void)printf(" %d %x", proto.pr_protocol, (int)sock);
	}
	(void)printf("\n");
}

/*
 * getinetproto --
 *	print name of protocol number
 */
getinetproto(number)
	int number;
{
	char *cp;

	switch(number) {
	case IPPROTO_IP:
		cp = "ip"; break;
	case IPPROTO_ICMP:
		cp ="icmp"; break;
	case IPPROTO_GGP:
		cp ="ggp"; break;
	case IPPROTO_TCP:
		cp ="tcp"; break;
	case IPPROTO_EGP:
		cp ="egp"; break;
	case IPPROTO_PUP:
		cp ="pup"; break;
	case IPPROTO_UDP:
		cp ="udp"; break;
	case IPPROTO_IDP:
		cp ="idp"; break;
	case IPPROTO_RAW:
		cp ="raw"; break;
	default:
		(void)printf(" %d", number);
		return;
	}
	(void)printf(" %s", cp);
}

getfname(filename)
	char *filename;
{
	struct stat statbuf;
	DEVS *cur;
	char *malloc();

	if (stat(filename, &statbuf)) {
		(void)fprintf(stderr, "fstat: %s: %s\n", strerror(errno),
		    filename);
		return(0);
	}
	if ((cur = (DEVS *)malloc(sizeof(DEVS))) == NULL) {
		(void)fprintf(stderr, "fstat: out of space.\n");
		exit(1);
	}
	cur->next = devs;
	devs = cur;

	cur->ino = statbuf.st_ino;
	cur->dev = statbuf.st_dev;
	cur->name = filename;
	return(1);
}

usage()
{
	(void)fprintf(stderr,
	    "usage: fstat [-u user] [-p pid] [filename ...]\n");
	exit(1);
}

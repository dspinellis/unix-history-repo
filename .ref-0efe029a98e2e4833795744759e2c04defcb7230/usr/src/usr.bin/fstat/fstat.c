/*-
 * Copyright (c) 1988 The Regents of the University of California.
 * All rights reserved.
 *
 * %sccs.include.redist.c%
 */

#ifndef lint
char copyright[] =
"@(#) Copyright (c) 1988 The Regents of the University of California.\n\
 All rights reserved.\n";
#endif /* not lint */

#ifndef lint
static char sccsid[] = "@(#)fstat.c	5.26 (Berkeley) %G%";
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
#include <ufs/quota.h>
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
#include <string.h>
#include <stdio.h>

#define	TEXT	-1
#define	CDIR	-2
#define	RDIR	-3
#define	TRACE	-4

typedef struct devs {
	struct	devs *next;
	long	fsid;
	ino_t	ino;
	char	*name;
} DEVS;
DEVS *devs;

struct  filestat {
	long	fsid;
	long	fileid;
	mode_t	mode;
	u_long	size;
	dev_t	rdev;
};

#ifdef notdef
struct nlist nl[] = {
	{ "" },
};
#endif

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
	int ch;
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
				fprintf(stderr, "%s: unknown uid\n",
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

	/* modify the following to make work on dead kernels */
	if (kvm_openfiles(NULL, NULL, NULL) == -1) {
		fprintf(stderr, "fstat: %s\n", kvm_geterr());
		exit(1);
	}
#ifdef notdef
	if (kvm_nlist(nl) != 0) {
		fprintf(stderr, "fstat: no namelist: %s\n", kvm_geterr());
		exit(1);
	}
#endif
	if (kvm_getprocs(what, arg) == -1) {
		fprintf(stderr, "fstat: %s\n", kvm_geterr());
		exit(1);
	}
	if (nflg)
fputs("USER     CMD        PID   FD  DEV    INUM       MODE SZ|DV", stdout);
	else
fputs("USER     CMD        PID   FD MOUNT      INUM MODE         SZ|DV", stdout);
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
	struct file file;
	struct user *up = kvm_getu(p);
	struct vnode *xvptr;
	extern char *user_from_uid();

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
	    kvm_read(&(p->p_textp->x_vptr), &xvptr,
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
	char *badtype = NULL;
	char *getmnton();
	extern char *devname();
	char mode[15];

	if (kvm_read((off_t)vp, &vn, sizeof (struct vnode)) != 
	    sizeof (struct vnode)) {
		dprintf(stderr, "can't read vnode at %x for pid %d\n",
			vp, Pid);
		return;
	}
	if (vn.v_type == VNON || vn.v_tag == VT_NON)
		badtype = "none";
	else if (vn.v_type == VBAD)
		badtype = "bad";
	else
		switch (vn.v_tag) {
		case VT_UFS:
			ufs_filestat(&vn, &fst);
			break;
		case VT_MFS:
			ufs_filestat(&vn, &fst);
			break;
		case VT_NFS:
			nfs_filestat(&vn, &fst);
			break;
		default: {
			static char unknown[10];
			sprintf(badtype = unknown, "?(%x)", vn.v_tag);
			break;;
		}
	}
	if (checkfile) {
		int fsmatch = 0;
		register DEVS *d;

		if (badtype)
			return;
		for (d = devs; d != NULL; d = d->next)
			if (d->fsid == fst.fsid) {
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
	if (badtype) {
		(void)printf(" -         -  %10s    -\n", badtype);
		return;
	}
	if (nflg)
		(void)printf(" %2d,%-2d", major(fst.fsid), minor(fst.fsid));
	else
		(void)printf(" %-8s", getmnton(vn.v_mount));
	if (nflg)
		(void)sprintf(mode, "%o", fst.mode);
	else
		strmode(fst.mode, mode);
	(void)printf(" %6d %10s", fst.fileid, mode);
	switch (vn.v_type) {
	case VBLK:
	case VCHR: {
		char *name;

		if (nflg || ((name = devname(fst.rdev, vn.v_type == VCHR ? 
		    S_IFCHR : S_IFBLK)) == NULL))
			printf("  %2d,%-2d", major(fst.rdev), minor(fst.rdev));
		else
			printf(" %6s", name);
		break;
	}
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

	fsp->fsid = ip->i_dev & 0xffff;
	fsp->fileid = (long)ip->i_number;
	fsp->mode = (mode_t)ip->i_mode;
	fsp->size = (u_long)ip->i_size;
	fsp->rdev = ip->i_rdev;
}

nfs_filestat(vp, fsp)
	struct vnode *vp;
	struct filestat *fsp;
{
	register struct nfsnode *np = VTONFS(vp);
	register mode_t mode;

	fsp->fsid = np->n_vattr.va_fsid;
	fsp->fileid = np->n_vattr.va_fileid;
	fsp->size = np->n_size;
	fsp->rdev = np->n_vattr.va_rdev;
	mode = (mode_t)np->n_vattr.va_mode;
	switch (vp->v_type) {
	case VREG:
		mode |= S_IFREG;
		break;
	case VDIR:
		mode |= S_IFDIR;
		break;
	case VBLK:
		mode |= S_IFBLK;
		break;
	case VCHR:
		mode |= S_IFCHR;
		break;
	case VLNK:
		mode |= S_IFLNK;
		break;
	case VSOCK:
		mode |= S_IFSOCK;
		break;
	case VFIFO:
		mode |= S_IFIFO;
		break;
	};
	fsp->mode = mode;
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
	bcopy(&mount.mnt_stat.f_mntonname[0], &mt->mntonname[0], MNAMELEN);
	mt->next = mhead;
	mhead = mt;
	return (mt->mntonname);
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
		goto bad;
	}

	/* fill in protosw entry */
	if (kvm_read((off_t)so.so_proto, (char *)&proto, sizeof(struct protosw))
	    != sizeof(struct protosw)) {
		dprintf(stderr, "can't read protosw at %x", so.so_proto);
		goto bad;
	}

	/* fill in domain */
	if (kvm_read((off_t)proto.pr_domain, (char *)&dom, sizeof(struct domain))
	    != sizeof(struct domain)) {
		dprintf(stderr, "can't read domain at %x\n", proto.pr_domain);
		goto bad;
	}

	/*
	 * grab domain name
	 * kludge "internet" --> "inet" for brevity
	 */
	if (dom.dom_family == AF_INET)
		strcpy(dname, "inet");
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
		printf("* %s ?%d", dname, so.so_type);
	else
		printf("* %s %s", dname, stypename[so.so_type]);

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
					goto bad;
				}
				printf(" %x", (int)inpcb.inp_ppcb);
			}
		}
		else if (so.so_pcb)
			printf(" %x", (int)so.so_pcb);
		break;
	case AF_UNIX:
		/* print address of pcb and connected pcb */
		if (so.so_pcb) {
			printf(" %x", (int)so.so_pcb);
			if (kvm_read((off_t)so.so_pcb, (char *)&unpcb, sizeof(struct unpcb))
			    != sizeof(struct unpcb)){
				dprintf(stderr, "can't read unpcb at %x\n",
					so.so_pcb);
				goto bad;
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
				printf(" %s %x", shoconn,
				    (int)unpcb.unp_conn);
			}
		}
		break;
	default:
		/* print protocol number and socket address */
		printf(" %d %x", proto.pr_protocol, (int)sock);
	}
	printf("\n");
	return;
bad:
	printf("* error\n");
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
		printf(" %d", number);
		return;
	}
	printf(" %s", cp);
}

getfname(filename)
	char *filename;
{
	struct stat statbuf;
	DEVS *cur;
	char *malloc();

	if (stat(filename, &statbuf)) {
		fprintf(stderr, "fstat: %s: %s\n", strerror(errno),
		    filename);
		return(0);
	}
	if ((cur = (DEVS *)malloc(sizeof(DEVS))) == NULL) {
		fprintf(stderr, "fstat: out of space.\n");
		exit(1);
	}
	cur->next = devs;
	devs = cur;

	cur->ino = statbuf.st_ino;
	cur->fsid = statbuf.st_dev & 0xffff;
	cur->name = filename;
	return(1);
}

usage()
{
	(void)fprintf(stderr,
	    "usage: fstat [-fnv] [-p pid] [-u user] [filename ...]\n");
	exit(1);
}

/*-
 * Copyright (c) 1980, 1991 The Regents of the University of California.
 * All rights reserved.
 *
 * %sccs.include.proprietary.c%
 */

#ifndef lint
char copyright[] =
"@(#) Copyright (c) 1980, 1991 The Regents of the University of California.\n\
 All rights reserved.\n";
#endif /* not lint */

#ifndef lint
static char sccsid[] = "@(#)pstat.c	5.40 (Berkeley) %G%";
#endif /* not lint */

#include <sys/param.h>
#include <sys/user.h>
#include <sys/proc.h>
#include <sys/time.h>
#include <sys/vnode.h>
#include <sys/map.h>
#define KERNEL
#include <sys/file.h>
#include <ufs/ufs/quota.h>
#include <ufs/ufs/inode.h>
#undef KERNEL
#define NFS
#include <sys/mount.h>
#undef NFS
#include <sys/stat.h>
#include <nfs/nfsnode.h>
/* #include <nfs/nfsv2.h> */
/* #include <nfs/nfs.h> */
#include <sys/ioctl.h>
#include <sys/tty.h>
#include <sys/conf.h>

#include <sys/sysctl.h>

#include <nlist.h>
#include <kvm.h>
#include <stdio.h>
#include <limits.h>
#include "pathnames.h"

#define mask(x)		(x&0377)
#define	clear(x)	((int)x &~ KERNBASE)

char	*nlistf	= NULL;
char	*memf	= NULL;

struct nlist nl[] = {
#define	SWAPMAP	0
	{ "_swapmap" },
#define	SNSWAPMAP 1
	{ "_nswapmap" },
#define	SDMMIN	2
	{ "_dmmin" },
#define	SDMMAX	3
	{ "_dmmax" },
#define	SNSWDEV	4
	{ "_nswdev" },
#define	SSWDEVT	5
	{ "_swdevt" },
#define NLMANDATORY SSWDEVT	/* names up to here are mandatory */
#define	SCONS	NLMANDATORY + 1
	{ "_cons" },
#define	SPTY	NLMANDATORY + 2
	{ "_pt_tty" },
#define	SNPTY	NLMANDATORY + 3
	{ "_npty" },
#ifdef vax
#define	SDZ	(SNPTY+1)
	{ "_dz_tty" },
#define	SNDZ	(SNPTY+2)
	{ "_dz_cnt" },
#define	SDMF	(SNPTY+3)
	{ "_dmf_tty" },
#define	SNDMF	(SNPTY+4)
	{ "_ndmf" },
#define	SDH	(SNPTY+5)
	{ "_dh11" },
#define	SNDH	(SNPTY+6)
	{ "_ndh11" },
#define	SDHU	(SNPTY+7)
	{ "_dhu_tty" },
#define	SNDHU	(SNPTY+8)
	{ "_ndhu" },
#define	SDMZ	(SNPTY+9)
	{ "_dmz_tty" },
#define	SNDMZ	(SNPTY+10)
	{ "_ndmz" },
#define	SQD	(SNPTY+11)
	{ "_qd_tty" },
#define	SNQD	(SNPTY+12)
	{ "_nNQD" },
#endif

#ifdef tahoe
#define	SVX	(SNPTY+1)
	{ "_vx_tty" },
#define	SNVX	(SNPTY+2)
	{ "_nvx" },
#define SMP	(SNPTY+3)
	{ "_mp_tty" },
#define SNMP	(SNPTY+4)
	{ "_nmp" },
#endif

#ifdef hp300
#define	SDCA	(SNPTY+1)
	{ "_dca_tty" },
#define	SNDCA	(SNPTY+2)
	{ "_ndca" },
#define	SDCM	(SNPTY+3)
	{ "_dcm_tty" },
#define	SNDCM	(SNPTY+4)
	{ "_ndcm" },
#define	SDCL	(SNPTY+5)
	{ "_dcl_tty" },
#define	SNDCL	(SNPTY+6)
	{ "_ndcl" },
#define	SITE	(SNPTY+7)
	{ "_ite_tty" },
#define	SNITE	(SNPTY+8)
	{ "_nite" },
#endif

#ifdef mips
#define SDC	(SNPTY+1)
	{ "_dc_tty" },
#define SNDC	(SNPTY+2)
	{ "_dc_cnt" },
#endif

	{ "" }
};

int	vnof;
int	txtf;
int	prcf;
int	ttyf;
int	usrf;
int	upid;
int	filf;
int	swpf;
int	totflg;
char	partab[1];
struct	cdevsw	cdevsw[1];
struct	bdevsw	bdevsw[1];
int	allflg;
int	nflg;
u_long	getword();
off_t	mkphys();
kvm_t	*kd;

#define V(x)	(u_long)(x)

main(argc, argv)
	int argc;
	char *argv[];
{
	extern char *optarg;
	extern int optind;
	int ch, ret;
	char buf[_POSIX2_LINE_MAX];

	while ((ch = getopt(argc, argv, "TafvikptU:sxnu")) != EOF)
		switch (ch) {
		case 'T':
			totflg++;
			break;
		case 'a':
			allflg++;
			/*FALLTHROUGH*/
		case 'p':
			prcf++;
			break;
		case 'f':
			filf++;
			break;
		case 'v':
		case 'i':
			vnof++;
			break;
		case 't':
			ttyf++;
			break;
		case 'U':
			usrf++;
			sscanf(optarg, "%d", &upid);
			break;
		case 's':
			swpf++;
			break;
		case 'x':
			txtf++;
			break;
		case 'n':
			nflg++;
			break;
		case 'u':
			fprintf(stderr, "pstat: use [ -U pid ] for -u\n");
			exit(1);
		case '?':
		default:
			usage();
		}
	argc -= optind;
	argv += optind;

	if (argc > 1)
		memf = argv[1];
	if (argc > 0)
		nlistf = argv[0];

	/*
	 * Discard setgid privileges if not the running kernel so that bad
	 * guys can't print interesting stuff from kernel memory.
	 */
	if (nlistf != NULL || memf != NULL)
		setgid(getgid());

	if ((kd = kvm_openfiles(nlistf, memf, NULL, O_RDONLY, buf)) == 0) {
		error("kvm_openfiles: %s", buf);
		exit(1);
	}
	if ((ret = kvm_nlist(kd, nl)) != 0) {
		int i, quit = 0;

		if (ret == -1) {
			error("kvm_nlist: %s", kvm_geterr(kd));
			exit(1);
		}
		for (i = 0; i <= NLMANDATORY; i++) {
			if (!nl[i].n_value) {
				quit = 1;
				error("undefined symbol: %s\n",
					nl[i].n_name);
			}
		}
		if (quit)
			exit(1);
	}
	if (!(filf | totflg | vnof | prcf | txtf | ttyf | usrf | swpf))
		usage();
	if (filf||totflg)
		dofile();
	if (vnof||totflg)
		dovnode();
	if (prcf||totflg)
		doproc();
	if (txtf||totflg)
		dotext();
	if (ttyf)
		dotty();
	if (usrf)
		dousr();
	if (swpf||totflg)
		doswap();
}

usage()
{

	fprintf(stderr,
	    "usage: pstat -[Tafiptsx] [-U [pid]] [system] [core]\n");
	exit(1);
}

struct e_vnode {
	struct vnode *avnode;
	struct vnode vnode;
};

dovnode()
{
	register struct e_vnode *e_vnodebase, *endvnode, *evp;
	register struct vnode *vp;
	register struct mount *maddr = NULL, *mp;
	int numvnodes;
	struct e_vnode *loadvnodes();
	struct mount *getmnt();

	e_vnodebase = loadvnodes(&numvnodes);
	if (totflg) {
		printf("%7d vnodes\n", numvnodes);
		return;
	}
	endvnode = e_vnodebase + numvnodes;
	printf("%d active vnodes\n", numvnodes);


#define ST	mp->mnt_stat
	for (evp = e_vnodebase; evp < endvnode; evp++) {
		vp = &evp->vnode;
		if (vp->v_mount != maddr) {
			/*
			 * New filesystem
			 */
			if ((mp = getmnt(vp->v_mount)) == NULL)
				continue;
			maddr = vp->v_mount;
			mount_print(mp);
			vnode_header();
			switch(ST.f_type) {
			case MOUNT_UFS:
			case MOUNT_MFS:
				ufs_header();
				break;
			case MOUNT_NFS:
				nfs_header();
				break;
			case MOUNT_NONE:
			case MOUNT_PC:
			default:
				break;
			}
			printf("\n");
		}
		vnode_print(evp->avnode, vp);
		switch(ST.f_type) {
		case MOUNT_UFS:
		case MOUNT_MFS:
			ufs_print(vp);
			break;
		case MOUNT_NFS:
			nfs_print(vp);
			break;
		case MOUNT_NONE:
		case MOUNT_PC:
		default:
			break;
		}
		printf("\n");
	}
	free(e_vnodebase);
}

vnode_header()
{
	printf("ADDR     TYP VFLAG  USE HOLD");
}

vnode_print(avnode, vp)
	struct vnode *avnode;
	struct vnode *vp;
{
	char *type, flags[16]; 
	char *fp = flags;
	register flag;

	/*
	 * set type
	 */
	switch(vp->v_type) {
	case VNON:
		type = "non"; break;
	case VREG:
		type = "reg"; break;
	case VDIR:
		type = "dir"; break;
	case VBLK:
		type = "blk"; break;
	case VCHR:
		type = "chr"; break;
	case VLNK:
		type = "lnk"; break;
	case VSOCK:
		type = "soc"; break;
	case VFIFO:
		type = "fif"; break;
	case VBAD:
		type = "bad"; break;
	default: 
		type = "unk"; break;
	}
	/*
	 * gather flags
	 */
	flag = vp->v_flag;
	if (flag & VROOT)
		*fp++ = 'R';
	if (flag & VTEXT)
		*fp++ = 'T';
	if (flag & VSYSTEM)
		*fp++ = 'S';
	if (flag & VXLOCK)
		*fp++ = 'L';
	if (flag & VXWANT)
		*fp++ = 'W';
	if (flag & VBWAIT)
		*fp++ = 'B';
	if (flag & VALIASED)
		*fp++ = 'A';
	if (flag == 0)
		*fp++ = '-';
	*fp = '\0';
	/*
	 * print it
	 */
	printf("%8x %s %5s %4d %4d",
		avnode, type, flags, vp->v_usecount, vp->v_holdcnt);
}

ufs_header() 
{
	printf(" FILEID IFLAG RDEV|SZ");
}

ufs_print(vp) 
	struct vnode *vp;
{
	struct inode inode, *ip = &inode;
	char flagbuf[16], *flags = flagbuf;
	register flag;
	char *name;
	mode_t type;
	extern char *devname();

	if (kvm_read(kd, V(VTOI(vp)), &inode, sizeof(struct inode)) != 
	    sizeof(struct inode)) {
		error("can't read inode for %x", vp);
		return;
	}
	flag = ip->i_flag;
	if (flag & ILOCKED)
		*flags++ = 'L';
	if (flag & IWANT)
		*flags++ = 'W';
	if (flag & IRENAME)
		*flags++ = 'R';
	if (flag & IUPD)
		*flags++ = 'U';
	if (flag & IACC)
		*flags++ = 'A';
	if (flag & ICHG)
		*flags++ = 'C';
	if (flag & IMOD)
		*flags++ = 'M';
	if (flag & ISHLOCK)
		*flags++ = 'S';
	if (flag & IEXLOCK)
		*flags++ = 'E';
	if (flag & ILWAIT)
		*flags++ = 'Z';
	if (flag == 0)
		*flags++ = '-';
	*flags = '\0';

	printf(" %6d %5s", ip->i_number, flagbuf);
	type = ip->i_mode & S_IFMT;
	if (type == S_IFCHR || type == S_IFBLK)
		if (nflg || ((name = devname(ip->i_rdev, type)) == NULL))
			printf("   %2d,%-2d", 
				major(ip->i_rdev), minor(ip->i_rdev));
		else
			printf(" %7s", name);
	else
		printf(" %7qd", ip->i_size);
}

nfs_header() 
{
	printf(" FILEID NFLAG RDEV|SZ");
}

nfs_print(vp) 
	struct vnode *vp;
{
	struct nfsnode nfsnode, *np = &nfsnode;
	char flagbuf[16], *flags = flagbuf;
	register flag;
	char *name;
	mode_t type;
	extern char *devname();

	if (kvm_read(kd, V(VTONFS(vp)), &nfsnode, sizeof(struct nfsnode)) != 
	    sizeof(struct nfsnode)) {
		error("can't read nfsnode for %x", vp);
		return;
	}
	flag = np->n_flag;
	if (flag & NFLUSHWANT)
		*flags++ = 'W';
	if (flag & NFLUSHINPROG)
		*flags++ = 'P';
	if (flag & NMODIFIED)
		*flags++ = 'M';
	if (flag & NWRITEERR)
		*flags++ = 'E';
	if (flag & NQNFSNONCACHE)
		*flags++ = 'X';
	if (flag & NQNFSWRITE)
		*flags++ = 'O';
	if (flag & NQNFSEVICTED)
		*flags++ = 'G';
	if (flag == 0)
		*flags++ = '-';
	*flags = '\0';

#define VT	np->n_vattr
	printf(" %6d %5s", VT.va_fileid, flagbuf);
	type = VT.va_mode & S_IFMT;
	if (type == S_IFCHR || type == S_IFBLK)
		if (nflg || ((name = devname(VT.va_rdev, type)) == NULL))
			printf("   %2d,%-2d", 
				major(VT.va_rdev), minor(VT.va_rdev));
		else
			printf(" %7s", name);
	else
		printf(" %7qd", np->n_size);
}
	
/*
 * Given a pointer to a mount structure in kernel space,
 * read it in and return a usable pointer to it.
 */
struct mount *
getmnt(maddr)
	struct mount *maddr;
{
	static struct mtab {
		struct mtab *next;
		struct mount *maddr;
		struct mount mount;
	} *mhead = NULL;
	register struct mtab *mt;

	for (mt = mhead; mt != NULL; mt = mt->next)
		if (maddr == mt->maddr)
			return (&mt->mount);
	if ((mt = (struct mtab *)malloc(sizeof (struct mtab))) == NULL) {
		error("out of memory");
		exit(1);
	}
	if (kvm_read(kd, V(maddr), &mt->mount, sizeof(struct mount)) != 
	    sizeof(struct mount)) {
		error("can't read mount table at %x", maddr);
		return (NULL);
	}
	mt->maddr = maddr;
	mt->next = mhead;
	mhead = mt;
	return (&mt->mount);
}

mount_print(mp)
	struct mount *mp;
{
	char *type = "unknown";
	register flags;

#define ST	mp->mnt_stat
	printf("*** MOUNT ");
	switch (ST.f_type) {
	case MOUNT_NONE:
		type = "none";
		break;
	case MOUNT_UFS:
		type = "ufs";
		break;
	case MOUNT_NFS:
		type = "nfs";
		break;
	case MOUNT_MFS:
		type = "mfs";
		break;
	case MOUNT_PC:
		type = "pc";
		break;
	}
	printf("%s %s on %s", type, ST.f_mntfromname, ST.f_mntonname);
	if (flags = mp->mnt_flag) {
		char *comma = "(";

		putchar(' ');
		/* user visable flags */
		if (flags & MNT_RDONLY) {
			printf("%srdonly", comma);
			flags &= ~MNT_RDONLY;
			comma = ",";
		}
		if (flags & MNT_SYNCHRONOUS) {
			printf("%ssynchronous", comma);
			flags &= ~MNT_SYNCHRONOUS;
			comma = ",";
		}
		if (flags & MNT_NOEXEC) {
			printf("%snoexec", comma);
			flags &= ~MNT_NOEXEC;
			comma = ",";
		}
		if (flags & MNT_NOSUID) {
			printf("%snosuid", comma);
			flags &= ~MNT_NOSUID;
			comma = ",";
		}
		if (flags & MNT_NODEV) {
			printf("%snodev", comma);
			flags &= ~MNT_NODEV;
			comma = ",";
		}
		if (flags & MNT_EXPORTED) {
			printf("%sexport", comma);
			flags &= ~MNT_EXPORTED;
			comma = ",";
		}
		if (flags & MNT_EXRDONLY) {
			printf("%sexrdonly", comma);
			flags &= ~MNT_EXRDONLY;
			comma = ",";
		}
		if (flags & MNT_LOCAL) {
			printf("%slocal", comma);
			flags &= ~MNT_LOCAL;
			comma = ",";
		}
		if (flags & MNT_QUOTA) {
			printf("%squota", comma);
			flags &= ~MNT_QUOTA;
			comma = ",";
		}
		/* filesystem control flags */
		if (flags & MNT_UPDATE) {
			printf("%supdate", comma);
			flags &= ~MNT_UPDATE;
			comma = ",";
		}
		if (flags & MNT_MLOCK) {
			printf("%slock", comma);
			flags &= ~MNT_MLOCK;
			comma = ",";
		}
		if (flags & MNT_MWAIT) {
			printf("%swait", comma);
			flags &= ~MNT_MWAIT;
			comma = ",";
		}
		if (flags & MNT_MPBUSY) {
			printf("%sbusy", comma);
			flags &= ~MNT_MPBUSY;
			comma = ",";
		}
		if (flags & MNT_MPWANT) {
			printf("%swant", comma);
			flags &= ~MNT_MPWANT;
			comma = ",";
		}
		if (flags & MNT_UNMOUNT) {
			printf("%sunmount", comma);
			flags &= ~MNT_UNMOUNT;
			comma = ",";
		}
		if (flags)
			printf("%sunknown_flags:%x", flags);
		printf(")");
	}
	printf("\n");
#undef ST
}

struct e_vnode *
loadvnodes(avnodes)
	int *avnodes;
{
	int mib[2];
	size_t copysize;
	struct e_vnode *vnodebase;
	struct e_vnode *kinfo_vnodes();

	if (memf != NULL) {
		/*
		 * do it by hand
		 */
		return (kinfo_vnodes(avnodes));
	}
	mib[0] = CTL_KERN;
	mib[1] = KERN_VNODE;
	if (sysctl(mib, 2, NULL, &copysize, NULL, 0) == -1) {
		syserror("can't get estimate from sysctl");
		exit(1);
	}
	if ((vnodebase = (struct e_vnode *)malloc(copysize)) == NULL) {
		error("out of memory");
		exit(1);
	}
	if (sysctl(mib, 2, vnodebase, &copysize, NULL, 0) == -1) {
		syserror("can't get vnode list");
		exit(1);
	}
	if (copysize % sizeof (struct e_vnode)) {
		error("vnode size mismatch");
		exit(1);
	}
	*avnodes = copysize / sizeof (struct e_vnode);

	return (vnodebase);
}

/*
 * simulate what a running kernel does in in kinfo_vnode
 */
struct e_vnode *
kinfo_vnodes(avnodes)
	int *avnodes;
{
	struct nlist vnl[] = {
#define V_NUMV	0
		{ "_numvnodes" },
#define V_ROOTFS 1
		{ "_rootfs" },
		{""}
	};
	int numvnodes;
	struct mount *rootfs, *mp, mount;
	char *vbuf, *evbuf, *bp;
	struct vnode *vp, vnode;
	int num;

#define VPTRSZ  sizeof (struct vnode *)
#define VNODESZ sizeof (struct vnode)
#define NVAL(indx)	vnl[(indx)].n_value

	if (kvm_nlist(kd, vnl) != 0) {
		error("nlist vnl: %s", kvm_geterr(kd));
		exit(1);
	}
	numvnodes = getword(NVAL(V_NUMV));
	if ((vbuf = (char *)malloc((numvnodes + 20) * (VPTRSZ + VNODESZ))) 
	    == NULL) {
		error("out of memory");
		exit(1);
	}
	bp = vbuf;
	evbuf = vbuf + (numvnodes + 20) * (VPTRSZ + VNODESZ);
	mp = rootfs = (struct mount *)getword(NVAL(V_ROOTFS));
	do {
		kvm_read(kd, V(mp), &mount, sizeof(mount));
		for (vp = mount.mnt_mounth; vp; vp = vnode.v_mountf) {
			kvm_read(kd, V(vp), &vnode, sizeof (vnode));
			if ((bp + VPTRSZ + VNODESZ) > evbuf) {
				/* XXX - should realloc */
				fprintf(stderr, "pstat: ran out of room for vnodes\n");
				exit(1);
			}
			bcopy(&vp, bp, VPTRSZ);
			bp += VPTRSZ;
			bcopy(&vnode, bp, VNODESZ);
			bp += VNODESZ;
			num++;
		}
		mp = mount.mnt_next;
	} while (mp != rootfs);
	*avnodes = num;
	return ((struct e_vnode *)vbuf);
}
	
	
u_long
getword(loc)
	int loc;
{
	u_long word;

	kvm_read(kd, V(loc), &word, sizeof (word));
	return (word);
}

putf(v, n)
{
	if (v)
		printf("%c", n);
	else
		printf(" ");
}

dotext()
{

	printf("no text table in this system\n");
}

doproc()
{
	if (!totflg)
		printf("pstat: -p no longer supported (use ps)\n");
}

char mesg[] = "  LINE RAW CAN OUT  HWT LWT     ADDR COL STATE  SESS  PGID DISC\n";
int ttyspace = 128;
struct tty *tty;

dotty()
{

	if ((tty = (struct tty *)malloc(ttyspace * sizeof(*tty))) == 0) {
		printf("pstat: out of memory\n");
		return;
	}
#ifndef hp300
	printf("1 cons\n");
	kvm_read(kd, V(nl[SCONS].n_value), tty, sizeof(*tty));
	printf(mesg);
	ttyprt(&tty[0], 0);
#endif
#ifdef vax
	if (nl[SNQD].n_type != 0) 
		doqdss();
	if (nl[SNDZ].n_type != 0)
		dottytype("dz", SDZ, SNDZ);
	if (nl[SNDH].n_type != 0)
		dottytype("dh", SDH, SNDH);
	if (nl[SNDMF].n_type != 0)
		dottytype("dmf", SDMF, SNDMF);
	if (nl[SNDHU].n_type != 0)
		dottytype("dhu", SDHU, SNDHU);
	if (nl[SNDMZ].n_type != 0)
		dottytype("dmz", SDMZ, SNDMZ);
#endif
#ifdef tahoe
	if (nl[SNVX].n_type != 0)
		dottytype("vx", SVX, SNVX);
	if (nl[SNMP].n_type != 0)
		dottytype("mp", SMP, SNMP);
#endif
#ifdef hp300
	if (nl[SNITE].n_type != 0)
		dottytype("ite", SITE, SNITE);
	if (nl[SNDCA].n_type != 0)
		dottytype("dca", SDCA, SNDCA);
	if (nl[SNDCM].n_type != 0)
		dottytype("dcm", SDCM, SNDCM);
	if (nl[SNDCL].n_type != 0)
		dottytype("dcl", SDCL, SNDCL);
#endif
#ifdef mips
	if (nl[SNDC].n_type != 0)
		dottytype("dc", SDC, SNDC);
#endif
	if (nl[SNPTY].n_type != 0)
		dottytype("pty", SPTY, SNPTY);
}

/* 
 * Special case the qdss: there are 4 ttys per qdss,
 * but only the first of each is used as a tty.  
 */
#ifdef vax
doqdss()
{
	int nqd;
	register struct tty *tp;

	kvm_read(kd, V(nl[SNQD].n_value), &nqd, sizeof(nqd));
	printf("%d qd\n", nqd);
	kvm_read(kd, V(nl[SQD].n_value), tty, nqd * sizeof(struct tty) * 4);
	printf(mesg);
	for (tp = tty; tp < &tty[nqd * 4]; tp += 4)
		ttyprt(tp, tp - tty);
}
#endif

dottytype(name, type, number)
char *name;
{
	int ntty;
	register struct tty *tp;
	extern char *realloc();

	if (tty == (struct tty *)0) 
		return;
	kvm_read(kd, V(nl[number].n_value), &ntty, sizeof(ntty));
	printf("%d %s %s\n", ntty, name, (ntty == 1) ? "line" :
	    "lines");
	if (ntty > ttyspace) {
		ttyspace = ntty;
		if ((tty = (struct tty *)realloc(tty, ttyspace * sizeof(*tty))) == 0) {
			printf("pstat: out of memory\n");
			return;
		}
	}
	kvm_read(kd, V(nl[type].n_value), tty, ntty * sizeof(struct tty));
	printf(mesg);
	for (tp = tty; tp < &tty[ntty]; tp++)
		ttyprt(tp, tp - tty);
}

struct {
	int flag;
	char val;
} ttystates[] = {
	TS_WOPEN,	'W',
	TS_ISOPEN,	'O',
	TS_CARR_ON,	'C',
	TS_TIMEOUT,	'T',
	TS_FLUSH,	'F',
	TS_BUSY,	'B',
	TS_ASLEEP,	'A',
	TS_XCLUDE,	'X',
	TS_TTSTOP,	'S',
	TS_TBLOCK,	'K',
	TS_ASYNC,	'Y',
	TS_BKSL,	'D',
	TS_ERASE,	'E',
	TS_LNCH,	'L',
	TS_TYPEN,	'P',
	TS_CNTTB,	'N',
	0,	0
};

ttyprt(atp, line)
struct tty *atp;
{
	register struct tty *tp;
	char state[20];
	register i, j;
	char *name;
	extern char *devname();
	pid_t pgid;

	tp = atp;
	if (nflg || tp->t_dev == 0 ||
	   (name = devname(tp->t_dev, S_IFCHR)) == NULL)
		printf("%7d ", line); 
	else
		printf("%7s ", name);
	printf("%2d %3d ", tp->t_rawq.c_cc, tp->t_canq.c_cc);
	printf("%3d %4d %3d %8x %3d ", tp->t_outq.c_cc, 
		tp->t_hiwat, tp->t_lowat, tp->t_addr, tp->t_col);
	for (i = j = 0; ttystates[i].flag; i++)
		if (tp->t_state&ttystates[i].flag)
			state[j++] = ttystates[i].val;
	if (j == 0)
		state[j++] = '-';
	state[j] = '\0';
	printf("%-4s %6x", state, (u_long)tp->t_session & ~KERNBASE);
	if (tp->t_pgrp == NULL || kvm_read(kd, V(&tp->t_pgrp->pg_id), &pgid, 
	    sizeof (pid_t)) != sizeof (pid_t))
		pgid = 0;
	printf("%6d ", pgid);
	switch (tp->t_line) {

	case TTYDISC:
		printf("term\n");
		break;

	case TABLDISC:
		printf("tab\n");
		break;

	case SLIPDISC:
		printf("slip\n");
		break;

	default:
		printf("%d\n", tp->t_line);
	}
}

/*
 * The user structure is going away.  What's left here won't
 * be around for long.
 */
dousr()
{

	printf("nothing left in user structure in this system\n");
}

oatoi(s)
char *s;
{
	register v;

	v = 0;
	while (*s)
		v = (v<<3) + *s++ - '0';
	return(v);
}

dofile()
{
	register struct file *fp;
	struct file *addr;
	char *buf;
	int len, maxfile, nfile;
	struct nlist fnl[] = {
#define	FNL_NFILE	0
		{"_nfiles"},
#define FNL_MAXFILE	1
		{"_maxfiles"},
		{""}
	};
	static char *dtypes[] = { "???", "inode", "socket" };

	if (kvm_nlist(kd, fnl) != 0) {
		error("kvm_nlist: no _nfiles or _maxfiles: %s", 
			kvm_geterr(kd));
		return;
	}
	kvm_read(kd, V(fnl[FNL_MAXFILE].n_value), &maxfile,
		sizeof (maxfile));
	if (totflg) {
		kvm_read(kd, V(fnl[FNL_NFILE].n_value), &nfile, sizeof (nfile));
		printf("%3d/%3d files\n", nfile, maxfile);
		return;
	}
	if (getfiles(&buf, &len) == -1)
		return;
	/*
	 * getfiles returns in malloc'd buf a pointer to the first file
	 * structure, and then an array of file structs (whose
	 * addresses are derivable from the previous entry)
	 */
	addr = *((struct file **)buf);
	fp = (struct file *)(buf + sizeof (struct file *));
	nfile = (len - sizeof (struct file *)) / sizeof (struct file);
	
	printf("%d/%d open files\n", nfile, maxfile);
	printf("   LOC   TYPE    FLG     CNT  MSG    DATA    OFFSET\n");
	for (; (char *)fp < buf + len; addr = fp->f_filef, fp++) {
		if ((unsigned)fp->f_type > DTYPE_SOCKET)
			continue;
		printf("%x ", addr);
		printf("%-8.8s", dtypes[fp->f_type]);
		putf(fp->f_flag&FREAD, 'R');
		putf(fp->f_flag&FWRITE, 'W');
		putf(fp->f_flag&FAPPEND, 'A');
#ifdef FSHLOCK	/* currently gone */
		putf(fp->f_flag&FSHLOCK, 'S');
		putf(fp->f_flag&FEXLOCK, 'X');
#else
		putf(0, ' ');
		putf(0, ' ');
#endif
		putf(fp->f_flag&FASYNC, 'I');
		printf("  %3d", fp->f_count);
		printf("  %3d", fp->f_msgcount);
		printf("  %8.1x", fp->f_data);
		if (fp->f_offset < 0)
			printf("  %x\n", fp->f_offset);
		else
			printf("  %ld\n", fp->f_offset);
	}
	free(buf);
}

getfiles(abuf, alen)
	char **abuf;
	int *alen;
{
	char *buf;
	int mib[2];
	size_t len;

	if (memf != NULL) {
		/*
		 * add emulation of KINFO_FILE here
		 */
		error("files on dead kernel, not impl\n");
		exit(1);
	}
	mib[0] = CTL_KERN;
	mib[1] = KERN_FILE;
	if (sysctl(mib, 2, NULL, &len, NULL, 0) == -1) {
		syserror("sysctl size estimate");
		return (-1);
	}
	if ((buf = (char *)malloc(len)) == NULL) {
		error("out of memory");
		return (-1);
	}
	if (sysctl(mib, 2, buf, &len, NULL, 0) == -1) {
		syserror("sysctl");
		return (-1);
	}
	*abuf = buf;
	*alen = len;
	return (0);
}


doswap()
{
	printf("swap statistics not yet supported in this system\n");
}

#include <varargs.h>

error(va_alist)
	va_dcl
{
	char *fmt;
	va_list ap;
	extern errno;

	fprintf(stderr, "pstat: ");
	va_start(ap);
	fmt = va_arg(ap, char *);
	(void) vfprintf(stderr, fmt, ap);
	va_end(ap);
	fprintf(stderr, "\n");
}

syserror(va_alist)
	va_dcl
{
	char *fmt;
	va_list ap;
	extern errno;

	fprintf(stderr, "pstat: ");
	va_start(ap);
	fmt = va_arg(ap, char *);
	(void) vfprintf(stderr, fmt, ap);
	va_end(ap);
	fprintf(stderr, ": %s\n", strerror(errno));
}

/*-
 * Copyright (c) 1980, 1991, 1993 The Regents of the University of California.
 * All rights reserved.
 *
 * %sccs.include.redist.c%
 */

#ifndef lint
char copyright[] =
"@(#) Copyright (c) 1980, 1991, 1993 The Regents of the University of California.\n\
 All rights reserved.\n";
#endif /* not lint */

#ifndef lint
static char sccsid[] = "@(#)pstat.c	5.41 (Berkeley) %G%";
#endif /* not lint */

#include <sys/param.h>
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
#include <sys/ioctl.h>
#include <sys/tty.h>
#include <sys/conf.h>

#include <sys/sysctl.h>

#include <nlist.h>
#include <kvm.h>
#include <stdio.h>
#include <stdlib.h>
#include <limits.h>
#include "pathnames.h"

struct nlist nl[] = {
#define VM_SWAPMAP	0
	{ "_swapmap" },	/* list of free swap areas */
#define VM_NSWAPMAP	1
	{ "_nswapmap" },/* size of the swap map */
#define VM_SWDEVT	2
	{ "_swdevt" },	/* list of swap devices and sizes */
#define VM_NSWAP	3
	{ "_nswap" },	/* size of largest swap device */
#define VM_NSWDEV	4
	{ "_nswdev" },	/* number of swap devices */
#define VM_DMMAX	5
	{ "_dmmax" },	/* maximum size of a swap block */
#define V_NUMV		6
	{ "_numvnodes" },
#define V_ROOTFS	7
	{ "_rootfs" },
#define	FNL_NFILE	8
	{"_nfiles"},
#define FNL_MAXFILE	9
	{"_maxfiles"},
#define NLMANDATORY FNL_MAXFILE	/* names up to here are mandatory */
#define	SCONS	NLMANDATORY + 1
	{ "_cons" },
#define	SPTY	NLMANDATORY + 2
	{ "_pt_tty" },
#define	SNPTY	NLMANDATORY + 3
	{ "_npty" },

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

int	usenumflag;
int	totalflag;
char	*nlistf	= NULL;
char	*memf	= NULL;
kvm_t	*kd;

#define	SVAR(var) __STRING(var)	/* to force expansion */
#define	KGET(idx, var) \
	KGET1(idx, &var, sizeof(var), SVAR(var))
#define	KGET1(idx, p, s, msg) \
	KGET2(nl[idx].n_value, p, s, msg)
#define	KGET2(addr, p, s, msg) \
	if (kvm_read(kd, (u_long)(addr), p, s) != s) \
		error("cannot read %s: %s", msg, kvm_geterr(kd))
#define	KGETRET(addr, p, s, msg) \
	if (kvm_read(kd, (u_long)(addr), p, s) != s) { \
		error("cannot read %s: %s", msg, kvm_geterr(kd)); \
		return (0); \
	}

main(argc, argv)
	int argc;
	char *argv[];
{
	extern char *optarg;
	extern int optind;
	int ch, ret;
	int swapflag = 0, vnodeflag = 0, ttyflag = 0, fileflag = 0;
	char buf[_POSIX2_LINE_MAX];

	while ((ch = getopt(argc, argv, "TM:N:finstv")) != EOF)
		switch (ch) {
		case 'M':
			memf = optarg;
			break;
		case 'N':
			nlistf = optarg;
			break;
		case 'T':
			totalflag++;
			break;
		case 'f':
			fileflag++;
			break;
		case 'v':
		case 'i':
			vnodeflag++;
			break;
		case 't':
			ttyflag++;
			break;
		case 's':
			swapflag++;
			break;
		case 'n':
			usenumflag++;
			break;
		default:
			usage();
		}
	argc -= optind;
	argv += optind;

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
	if (!(fileflag | vnodeflag | ttyflag | swapflag | totalflag))
		usage();
	if (fileflag || totalflag)
		dofile();
	if (vnodeflag || totalflag)
		dovnode();
	if (ttyflag)
		dotty();
	if (swapflag || totalflag)
		doswap();
}

usage()
{

	fprintf(stderr,
	    "usage: pstat -Tfnstv [system] [core]\n");
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
	if (totalflag) {
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


	KGETRET(VTOI(vp), &inode, sizeof(struct inode), "vnode's inode");
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
		if (usenumflag || ((name = devname(ip->i_rdev, type)) == NULL))
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

	KGETRET(VTONFS(vp), &nfsnode, sizeof(nfsnode), "vnode's nfsnode");
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
		if (usenumflag || ((name = devname(VT.va_rdev, type)) == NULL))
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
	KGETRET(maddr, &mt->mount, sizeof(struct mount), "mount table");
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
	int numvnodes;
	struct mount *rootfs, *mp, mount;
	char *vbuf, *evbuf, *bp;
	struct vnode *vp, vnode;
	int num;

#define VPTRSZ  sizeof (struct vnode *)
#define VNODESZ sizeof (struct vnode)

	KGET(V_NUMV, numvnodes);
	if ((vbuf = (char *)malloc((numvnodes + 20) * (VPTRSZ + VNODESZ))) 
	    == NULL) {
		error("out of memory");
		exit(1);
	}
	bp = vbuf;
	evbuf = vbuf + (numvnodes + 20) * (VPTRSZ + VNODESZ);
	KGET(V_ROOTFS, rootfs);
	mp = rootfs;
	do {
		KGET2(mp, &mount, sizeof(mount), "mount entry");
		for (vp = mount.mnt_mounth; vp; vp = vnode.v_mountf) {
			KGET2(vp, &vnode, sizeof(vnode), "vnode");
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
	
char hdr[]="  LINE RAW CAN OUT  HWT LWT     ADDR COL STATE  SESS  PGID DISC\n";
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
	KGET(SCONS, *tty);
	printf(hdr);
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

dottytype(name, type, number)
char *name;
{
	int ntty;
	register struct tty *tp;

	if (tty == (struct tty *)0) 
		return;
	KGET(number, ntty);
	printf("%d %s %s\n", ntty, name, (ntty == 1) ? "line" :
	    "lines");
	if (ntty > ttyspace) {
		ttyspace = ntty;
		if ((tty = (struct tty *)realloc(tty, ttyspace * sizeof(*tty))) == 0) {
			printf("pstat: out of memory\n");
			return;
		}
	}
	KGET1(type, tty, ntty * sizeof(struct tty), "tty structs");
	printf(hdr);
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
	pid_t pgid;

	tp = atp;
	if (usenumflag || tp->t_dev == 0 ||
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
	pgid = 0;
	if (tp->t_pgrp != NULL)
		KGET2(&tp->t_pgrp->pg_id, &pgid, sizeof (pid_t), "pgid");
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

dofile()
{
	register struct file *fp;
	struct file *addr;
	char *buf, flagbuf[16], *fbp;
	int len, maxfile, nfile;
	static char *dtypes[] = { "???", "inode", "socket" };

	KGET(FNL_MAXFILE, maxfile);
	if (totalflag) {
		KGET(FNL_NFILE, nfile);
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
		fbp = flagbuf;
		if (fp->f_flag & FREAD)
			*fbp++ = 'R';
		if (fp->f_flag & FWRITE)
			*fbp++ = 'W';
		if (fp->f_flag & FAPPEND)
			*fbp++ = 'A';
#ifdef FSHLOCK	/* currently gone */
		if (fp->f_flag & FSHLOCK)
			*fbp++ = 'S';
		if (fp->f_flag & FEXLOCK)
			*fbp++ = 'X';
#endif
		if (fp->f_flag & FASYNC)
			*fbp++ = 'I';
		*fbp = '\0';
		printf("%6s  %3d", flagbuf, fp->f_count);
		printf("  %3d", fp->f_msgcount);
		printf("  %8.1x", fp->f_data);
		if (fp->f_offset < 0)
			printf("  %qx\n", fp->f_offset);
		else
			printf("  %qd\n", fp->f_offset);
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

/*
 * doswap is based on a program called swapinfo written
 * by Kevin Lahey <kml@rokkaku.atl.ga.us>.
 */
doswap()
{
	char *header;
	int hlen, nswap, nswdev, dmmax, nswapmap;
	int s, e, div, i, avail, nfree, npfree, used;
	struct swdevt *sw;
	long blocksize, *perdev;
	struct map *swapmap, *kswapmap;
	struct mapent *mp;

	KGET(VM_NSWAP, nswap);
	KGET(VM_NSWDEV, nswdev);
	KGET(VM_DMMAX, dmmax);
	KGET(VM_NSWAPMAP, nswapmap);
	KGET(VM_SWAPMAP, kswapmap);	/* kernel `swapmap' is a pointer */
	if ((sw = (struct swdevt *)malloc(nswdev * sizeof(*sw))) == NULL ||
	    (perdev = (long *)malloc(nswdev * sizeof(*perdev))) == NULL ||
	    (mp = (struct mapent *)malloc(nswapmap * sizeof(*mp))) == NULL)
		err(1, "malloc");
	KGET1(VM_SWDEVT, sw, nswdev * sizeof(*sw), "swdevt");
	KGET2((long)kswapmap, mp, nswapmap * sizeof(*mp), "swapmap");

	/* first entry in map is `struct map'; rest are mapent's */
	swapmap = (struct map *)mp;
	if (nswapmap != swapmap->m_limit - (struct mapent *)kswapmap)
		errx(1, "panic: nswapmap goof");

	/*
	 * Count up swap space.
	 */
	nfree = 0;
	bzero(perdev, nswdev * sizeof(*perdev));
	for (mp++; mp->m_addr != 0; mp++) {
		s = mp->m_addr;			/* start of swap region */
		e = mp->m_addr + mp->m_size;	/* end of region */
		nfree += mp->m_size;

		/*
		 * Swap space is split up among the configured disks.
		 * The first dmmax blocks of swap space some from the
		 * first disk, the next dmmax blocks from the next, 
		 * and so on.  The list of free space joins adjacent
		 * free blocks, ignoring device boundries.  If we want
		 * to keep track of this information per device, we'll
		 * just have to extract it ourselves.
		 */

		/* calculate first device on which this falls */
		i = (s / dmmax) % nswdev;
		while (s < e) {		/* XXX this is inefficient */
			int bound = roundup(s+1, dmmax);

			if (bound > e)
				bound = e;
			perdev[i] += bound - s;
			if (++i >= nswdev)
				i = 0;
			s = bound;
		}
	}

	header = getbsize(&hlen, &blocksize);
	if (!totalflag)
		(void)printf("%-10s %*s %10s %10s %10s\n",
		    "Device", hlen, header, "Used", "Available", "Capacity");
	div = blocksize / 512;
	avail = npfree = 0;
	for (i = 0; i < nswdev; i++) {
		int xsize, xfree;

		if (!totalflag)
			(void)printf("/dev/%-5s %*d ",
			    devname(sw[i].sw_dev, S_IFBLK),
			    hlen, sw[i].sw_nblks / div);

		/*
		 * Don't report statistics for partitions which have not
		 * yet been activated via swapon(8).
		 */
		if (!sw[i].sw_freed) {
			if (totalflag)
				continue;
			(void)printf(" *** not available for swapping ***\n");
			continue;
		}
		xsize = sw[i].sw_nblks;
		xfree = perdev[i];
		used = xsize - xfree;
		npfree++;
		avail += xsize;
		if (totalflag)
			continue;
		(void)printf("%10d %10d %7.0f%%\n", 
		    used / div, xfree / div,
		    (double)used / (double)xsize * 100.0);
	}

	/* 
	 * If only one partition has been set up via swapon(8), we don't
	 * need to bother with totals.
	 */
	used = avail - nfree;
	if (totalflag) {
		printf("%dM/%dM swap space\n", used / 2048, avail / 2048);
		return;
	}
	if (npfree > 1) {
		(void)printf("%-10s %*d %10d %10d %7.0f%%\n",
		    "Total", hlen, avail / div, used / div, nfree / div,
		    (double)used / (double)avail * 100.0);
	}
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

/*
 * Copyright (c) 1980 Regents of the University of California.
 * All rights reserved.  The Berkeley software License Agreement
 * specifies the terms and conditions for redistribution.
 */

#ifndef lint
char copyright[] =
"@(#) Copyright (c) 1980 Regents of the University of California.\n\
 All rights reserved.\n";
#endif /* not lint */

#ifndef lint
static char sccsid[] = "@(#)pstat.c	5.28 (Berkeley) %G%";
#endif /* not lint */

/*
 * Print system stuff
 */
#include <sys/param.h>
#include <sys/user.h>
#include <sys/proc.h>
#include <sys/text.h>
#include <sys/time.h>
#include <sys/vnode.h>
#include <sys/map.h>
#define KERNEL
#define NFS
#include <sys/file.h>
#include <sys/mount.h>
#include <ufs/quota.h>
#include <ufs/inode.h>
#include <sys/stat.h>
#include <nfs/nfsv2.h>
#include <nfs/nfs.h>
#include <nfs/nfsnode.h>
#include <sys/ioctl.h>
#include <sys/tty.h>
#undef KERNEL
#include <sys/conf.h>
#include <sys/vm.h>
#include <machine/pte.h>

#include <kvm.h>
#include <nlist.h>
#include <stdio.h>
#include "pathnames.h"

#define mask(x)		(x&0377)
#define	clear(x)	((int)x &~ KERNBASE)

char	*fnlist	= NULL;
char	*fcore	= NULL;

struct nlist nl[] = {
#define	STEXT	0
	{ "_text" },
#define	SCONS	1
	{ "_cons" },
#define	SPROC	2
	{ "_proc" },
#define	SFIL	3
	{ "_file" },
#define	SWAPMAP	4
	{ "_swapmap" },
#define	SNPROC	5
	{ "_nproc" },
#define	SNTEXT	6
	{ "_ntext" },
#define	SNFILE	7
	{ "_nfile" },
#define	SNSWAPMAP 8
	{ "_nswapmap" },
#define	SPTY	9
	{ "_pt_tty" },
#define	SDMMIN	10
	{ "_dmmin" },
#define	SDMMAX	11
	{ "_dmmax" },
#define	SNSWDEV	12
	{ "_nswdev" },
#define	SSWDEVT	13
	{ "_swdevt" },
#define	SNPTY	14
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
	{ "_dcm_cnt" },
#define	SDCL	(SNPTY+5)
	{ "_dcl_tty" },
#define	SNDCL	(SNPTY+6)
	{ "_ndcl" },
#define	SITE	(SNPTY+7)
	{ "_ite_tty" },
#define	SNITE	(SNPTY+8)
	{ "_nite" },
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

char	*Program;

main(argc, argv)
	int argc;
	char **argv;
{
	extern char *optarg;
	extern int optind;
	int ch;

        Program = argv[0];
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
			fprintf(stderr, "usage: pstat -[Tafiptsx] [-U [pid]] [system] [core]\n");
			exit(1);
		}
	argc -= optind;
	argv += optind;

	if (argc > 1)
		fcore = argv[1];
	if (argc > 0)
		fnlist = argv[0];
	if (kvm_openfiles(fnlist, fcore, NULL) == -1) {
		syserror("kvm_openfiles: %s", kvm_geterr());
		exit(1);
	}
	if (kvm_nlist(nl) != 0) {
		syserror("kvm_nlist: %s", kvm_geterr());
		/*
		exit(1);
		*/
	}
	if (!(filf | totflg | vnof | prcf | txtf | ttyf | usrf | swpf)) {
		printf("pstat: one or more of -[aivxptfsU] is required\n");
		exit(1);
	}
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

struct e_vnode {
	struct vnode *avnode;
	struct vnode vnode;
};

dovnode()
{
	register struct e_vnode *e_vnodebase, *endvnode, *evp;
	register struct vnode *vp;
	register struct mount *maddr = NULL, *mp;
	register struct inode *ip;
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
	if (flag & VXLOCK)
		*fp++ = 'L';
	if (flag & VXWANT)
		*fp++ = 'W';
	if (flag & VEXLOCK)
		*fp++ = 'E';
	if (flag & VSHLOCK)
		*fp++ = 'S';
	if (flag & VLWAIT)
		*fp++ = 'T';
	if (flag & VALIASED)
		*fp++ = 'A';
	if (flag & VBWAIT)
		*fp++ = 'B';
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
	struct inode *ip = VTOI(vp);
	char flagbuf[16], *flags = flagbuf;
	register flag;
	char *name;
	mode_t type;
	extern char *devname();

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
		printf(" %7d", ip->i_size);
}

nfs_header() 
{
	printf(" FILEID NFLAG RDEV|SZ");
}

nfs_print(vp) 
	struct vnode *vp;
{
	struct nfsnode *np = VTONFS(vp);
	char flagbuf[16], *flags = flagbuf;
	register flag;
	char *name;
	mode_t type;
	extern char *devname();

	flag = np->n_flag;
	if (flag & NLOCKED)
		*flags++ = 'L';
	if (flag & NWANT)
		*flags++ = 'W';
	if (flag & NMODIFIED)
		*flags++ = 'M';
	if (flag & NWRITEERR)
		*flags++ = 'E';
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
		printf(" %7d", np->n_size);
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
	if (kvm_read((off_t)maddr, &mt->mount, sizeof(struct mount)) != 
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
	int ret, copysize, i;
	struct e_vnode *vnodebase;

	if (fcore != NULL) {
		error("vnodes on dead kernel, not impl yet\n");
		exit(1);
	}
	if ((ret = getkerninfo(KINFO_VNODE, NULL, NULL, 0)) == -1) {
		syserror("can't get estimate for kerninfo");
		exit(1);
	}
	copysize = ret;
	if ((vnodebase = (struct e_vnode *)malloc(copysize)) 
	     == NULL) {
		error("out of memory");
		exit(1);
	}
	if ((ret = getkerninfo(KINFO_VNODE, vnodebase, &copysize, 0)) 
	     == -1) {
		syserror("can't get vnode list");
		exit(1);
	}
	if (copysize % sizeof (struct e_vnode)) {
		error("vnode size mismatch");
		error(1);
	}
	*avnodes = copysize / sizeof (struct e_vnode);

	return (vnodebase);
}

u_long
getword(loc)
	off_t loc;
{
	u_long word;

	kvm_read(loc, &word, sizeof (word));
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
	register struct text *xp;
	int ntext;
	struct text *xtext, *atext;
	int ntx, ntxca;

	ntx = ntxca = 0;
	ntext = getword(nl[SNTEXT].n_value);
	xtext = (struct text *)calloc(ntext, sizeof (struct text));
	atext = (struct text *)getword(nl[STEXT].n_value);
	if (ntext < 0 || ntext > 10000) {
		fprintf(stderr, "number of texts is preposterous (%d)\n",
			ntext);
		return;
	}
	if (xtext == NULL) {
		fprintf(stderr, "can't allocate memory for text table\n");
		return;
	}
	kvm_read(atext, xtext, ntext * sizeof (struct text));
	for (xp = xtext; xp < &xtext[ntext]; xp++) {
		if (xp->x_vptr != NULL)
			ntxca++;
		if (xp->x_count != 0)
			ntx++;
	}
	if (totflg) {
		printf("%3d/%3d texts active, %3d used\n", ntx, ntext, ntxca);
		return;
	}
	printf("%d/%d active texts, %d used\n", ntx, ntext, ntxca);
	printf("\
   LOC   FLAGS DADDR     CADDR  RSS SIZE     VPTR   CNT CCNT      FORW     BACK\n");
	for (xp = xtext; xp < &xtext[ntext]; xp++) {
		if (xp->x_vptr == NULL)
			continue;
		printf("%8.1x", atext + (xp - xtext));
		printf(" ");
		putf(xp->x_flag&XPAGV, 'P');
		putf(xp->x_flag&XTRC, 'T');
		putf(xp->x_flag&XWRIT, 'W');
		putf(xp->x_flag&XLOAD, 'L');
		putf(xp->x_flag&XLOCK, 'K');
		putf(xp->x_flag&XWANT, 'w');
		printf("%5x", xp->x_daddr[0]);
		printf("%10x", xp->x_caddr);
		printf("%5d", xp->x_rssize);
		printf("%5d", xp->x_size);
		printf("%10.1x", xp->x_vptr);
		printf("%5d", xp->x_count&0377);
		printf("%5d", xp->x_ccount);
		printf("%10x", xp->x_forw);
		printf("%9x", xp->x_back);
		printf("\n");
	}
	free(xtext);
}

doproc()
{
	struct proc *xproc, *aproc;
	int nproc;
	register struct proc *pp;
	register loc, np;
	struct pte apte;

	nproc = getword(nl[SNPROC].n_value);
	xproc = (struct proc *)calloc(nproc, sizeof (struct proc));
	aproc = (struct proc *)getword(nl[SPROC].n_value);
	if (nproc < 0 || nproc > 100000) {
		fprintf(stderr, "number of procs is preposterous (%d)\n",
			nproc);
		return;
	}
	if (xproc == NULL) {
		fprintf(stderr, "can't allocate memory for proc table\n");
		return;
	}
	kvm_read(aproc, xproc, nproc * sizeof (struct proc));
	np = 0;
	for (pp=xproc; pp < &xproc[nproc]; pp++)
		if (pp->p_stat)
			np++;
	if (totflg) {
		printf("%3d/%3d processes\n", np, nproc);
		return;
	}
	printf("%d/%d processes\n", np, nproc);
	printf("   LOC    S        F POIP PRI      SIG  UID SLP TIM  CPU  NI    PID   PPID    ADDR   RSS SRSS SIZE    WCHAN    LINK   TEXTP\n");
	for (pp=xproc; pp<&xproc[nproc]; pp++) {
		if (pp->p_stat==0 && allflg==0)
			continue;
		printf("%8x", aproc + (pp - xproc));
		printf(" %2d", pp->p_stat);
		printf(" %8x", pp->p_flag);
		printf(" %4d", pp->p_poip);
		printf(" %3d", pp->p_pri);
		printf(" %8x", pp->p_sig);
		printf(" %4d", pp->p_uid);
		printf(" %3d", pp->p_slptime);
		printf(" %3d", pp->p_time);
		printf(" %4d", pp->p_cpu&0377);
		printf(" %3d", pp->p_nice);
		printf(" %6d", pp->p_pid);
		printf(" %6d", pp->p_ppid);
		/* 
		if (pp->p_flag & SLOAD) {
			kvm_read(pp->p_addr, &apte, sizeof(apte));
			printf(" %8x", apte.pg_pfnum);
		} else
			printf(" %8x", pp->p_swaddr);
		*/
		printf(" %4x", pp->p_rssize);
		printf(" %4x", pp->p_swrss);
		printf(" %5x", pp->p_dsize+pp->p_ssize);
		printf(" %7x", clear(pp->p_wchan));
		printf(" %7x", clear(pp->p_link));
		printf(" %7x", clear(pp->p_textp));
		printf("\n");
	}
	free(xproc);
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
	kvm_read((long)nl[SCONS].n_value, tty, sizeof(*tty));
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

	kvm_read((long)nl[SNQD].n_value, &nqd, sizeof(nqd));
	printf("%d qd\n", nqd);
	kvm_read((long)nl[SQD].n_value, tty, nqd * sizeof(struct tty) * 4);
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
	kvm_read((long)nl[number].n_value, &ntty, sizeof(ntty));
	printf("%d %s lines\n", ntty, name);
	if (ntty > ttyspace) {
		ttyspace = ntty;
		if ((tty = (struct tty *)realloc(tty, ttyspace * sizeof(*tty))) == 0) {
			printf("pstat: out of memory\n");
			return;
		}
	}
	kvm_read((long)nl[type].n_value, tty, ntty * sizeof(struct tty));
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
	TS_HUPCLS,	'H',
	TS_TBLOCK,	'K',
	TS_RCOLL,	'R',
	TS_WCOLL,	'I',	/* running short on letters ! */
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
	if (nflg || tp->t_dev == 0 || 	/* XXX */
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
	if (tp->t_pgrp == NULL || kvm_read(&tp->t_pgrp->pg_id, &pgid, 
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

dousr()
{
	register struct user *up;
	register i, j, *ip;
	register struct nameidata *nd;
	struct proc *p;
	int ret;

	if ((ret = kvm_getprocs(KINFO_PROC_PID, upid)) != 1) {
		if (ret == -1)
			error("kvm_getproc: %s", kvm_geterr());
		else
			error("can't locate process %d", upid);
		return (1);
	}
	if ((p = kvm_nextproc()) == NULL) {
		error("kvm_nextproc: %s", kvm_geterr());
		return (1);
	}
	if ((up = kvm_getu(p)) == NULL) {
		error("kvm_getu: %s", kvm_geterr());
		return (1);
	}
	nd = &up->u_nd;
	printf("pcb");
	ip = (int *)&up->u_pcb;
	i = 0;
	while (ip < (int *)((char *)&up->u_pcb + sizeof (struct pcb))) {
		if (i%4 == 0)
			putchar('\t');
		printf("%#10x ", *ip++);
		if (i%4 == 3)
			putchar('\n');
		i++;
	}
	if (i%4)
		putchar('\n');
	printf("procp\t%#x\n", up->u_procp);
	printf("ar0\t%#x\n", up->u_ar0);
	printf("sizes\ttext %d data %d stack %d\n", 
		up->u_tsize, up->u_dsize, up->u_ssize);
	/* DMAPS */
	printf("ssave");
	for (i=0; i<sizeof(label_t)/sizeof(int); i++) {
		if (i%5==0)
			printf("\t");
		printf("%#11x", up->u_ssave.val[i]);
		if (i%5==4)
			printf("\n");
	}
	if (i%5)
		printf("\n");
	printf("odsize\t%#x\n", up->u_odsize);
	printf("ossize\t%#x\n", up->u_ossize);
	printf("outime\t%d\n", up->u_outime);
	printf("mmap\t%#x\n", up->u_mmap);
	printf("sigs");
	for (i=0; i<NSIG; i++) {
		if (i % 8 == 0)
			printf("\t");
		printf("%#x ", up->u_signal[i]);
		if (i % 8 == 7)
			printf("\n");
	}
	if (i % 8)
		printf("\n");
	printf("sigmask");
	for (i=0; i<NSIG; i++) {
		if (i % 8 == 0)
			printf("\t");
		printf("%#x ", up->u_sigmask[i]);
		if (i % 8 == 7)
			printf("\n");
	}
	if (i % 8)
		printf("\n");
	printf("sigonstack\t%#x\n", up->u_sigonstack);
	printf("sigintr\t%#x\n", up->u_sigintr);
	printf("oldmask\t%#x\n", up->u_oldmask);
	printf("sigstack\t%#x %#x\n", 
		up->u_sigstack.ss_sp, up->u_sigstack.ss_onstack);
	printf("sig\t%#x\n", up->u_sig);
	printf("code\t%#x\n", up->u_code);
	printf("file");
	for (i=0; i<NOFILE; i++) {
		if (i % 6 == 0)
			printf("\t");
		printf("%#11x", up->u_ofile[i]);
		if (i % 6 == 5)
			printf("\n");
	}
	if (i % 6)
		printf("\n");
	printf("pofile");
	for (i=0; i<NOFILE; i++) {
		if (i % 6 == 0)
			printf("\t");
		printf("%#11x", up->u_pofile[i]);
		if (i % 6 == 5)
			printf("\n");
	}
	if (i % 6)
		printf("\n");
	printf("lastfile\t%d\n", up->u_lastfile);
	printf("cmask\t%#o\n", up->u_cmask);
	/* RUSAGES */
	/* TIMERS */
	printf("start\t%ld secs %ld usecs\n", 
		up->u_start.tv_sec, up->u_start.tv_usec);
	printf("acflag\t%#x\n", up->u_acflag);
	printf("prof\t%#x %#x %#x %#x\n", up->u_prof.pr_base, up->u_prof.pr_size,
	    up->u_prof.pr_off, up->u_prof.pr_scale);
	printf("ru\t");
	ip = (int *)&up->u_ru;
	for (i = 0; i < sizeof(up->u_ru)/sizeof(int); i++)
		printf("%ld ", ip[i]);
	printf("\n");
	ip = (int *)&up->u_cru;
	printf("cru\t");
	for (i = 0; i < sizeof(up->u_cru)/sizeof(int); i++)
		printf("%ld ", ip[i]);
	printf("\n");
	/* NAMEI */
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
	int nfile;
	struct file *xfile, *afile;
	register struct file *fp;
	register nf;
	int loc;
	static char *dtypes[] = { "???", "inode", "socket" };

	nf = 0;
	nfile = getword(nl[SNFILE].n_value);
	xfile = (struct file *)calloc(nfile, sizeof (struct file));
	afile = (struct file *)getword(nl[SFIL].n_value);
	if (nfile < 0 || nfile > 100000) {
		fprintf(stderr, "number of files is preposterous (%d)\n",
			nfile);
		return;
	}
	if (xfile == NULL) {
		fprintf(stderr, "can't allocate memory for file table\n");
		return;
	}
	kvm_read(afile, xfile, nfile * sizeof (struct file));
	for (fp=xfile; fp < &xfile[nfile]; fp++)
		if (fp->f_count)
			nf++;
	if (totflg) {
		printf("%3d/%3d files\n", nf, nfile);
		return;
	}
	printf("%d/%d open files\n", nf, nfile);
	printf("   LOC   TYPE    FLG     CNT  MSG    DATA    OFFSET\n");
	for (fp=xfile,loc=(int)afile; fp < &xfile[nfile]; fp++,loc+=sizeof(xfile[0])) {
		if (fp->f_count==0)
			continue;
		printf("%x ", loc);
		if (fp->f_type <= DTYPE_SOCKET)
			printf("%-8.8s", dtypes[fp->f_type]);
		else
			printf("%8d", fp->f_type);
		putf(fp->f_flag&FREAD, 'R');
		putf(fp->f_flag&FWRITE, 'W');
		putf(fp->f_flag&FAPPEND, 'A');
		putf(fp->f_flag&FSHLOCK, 'S');
		putf(fp->f_flag&FEXLOCK, 'X');
		putf(fp->f_flag&FASYNC, 'I');
		printf("  %3d", mask(fp->f_count));
		printf("  %3d", mask(fp->f_msgcount));
		printf("  %8.1x", fp->f_data);
		if (fp->f_offset < 0)
			printf("  %x\n", fp->f_offset);
		else
			printf("  %ld\n", fp->f_offset);
	}
	free(xfile);
}

int dmmin, dmmax, nswdev;

doswap()
{
	struct proc *proc;
	int nproc;
	struct text *xtext;
	int ntext;
	struct map *swapmap;
	int nswapmap;
	struct swdevt *swdevt, *sw;
	register struct proc *pp;
	int nswap, used, tused, free, waste;
	int db, sb;
	register struct mapent *me;
	register struct text *xp;
	int i, j;
	long rmalloc();

	nproc = getword(nl[SNPROC].n_value);
	ntext = getword(nl[SNTEXT].n_value);
	if (nproc < 0 || nproc > 10000 || ntext < 0 || ntext > 10000) {
		fprintf(stderr, "number of procs/texts is preposterous (%d, %d)\n",
			nproc, ntext);
		return;
	}
	proc = (struct proc *)calloc(nproc, sizeof (struct proc));
	if (proc == NULL) {
		fprintf(stderr, "can't allocate memory for proc table\n");
		exit(1);
	}
	xtext = (struct text *)calloc(ntext, sizeof (struct text));
	if (xtext == NULL) {
		fprintf(stderr, "can't allocate memory for text table\n");
		exit(1);
	}
	nswapmap = getword(nl[SNSWAPMAP].n_value);
	swapmap = (struct map *)calloc(nswapmap, sizeof (struct map));
	if (swapmap == NULL) {
		fprintf(stderr, "can't allocate memory for swapmap\n");
		exit(1);
	}
	nswdev = getword(nl[SNSWDEV].n_value);
	swdevt = (struct swdevt *)calloc(nswdev, sizeof (struct swdevt));
	if (swdevt == NULL) {
		fprintf(stderr, "can't allocate memory for swdevt table\n");
		exit(1);
	}
	kvm_read(nl[SSWDEVT].n_value, swdevt,
		nswdev * sizeof (struct swdevt));
	kvm_read(getword(nl[SPROC].n_value), proc,
		nproc * sizeof (struct proc));
	kvm_read(getword(nl[STEXT].n_value), xtext,
		ntext * sizeof (struct text));
	kvm_read(getword(nl[SWAPMAP].n_value), swapmap,
		nswapmap * sizeof (struct map));

	swapmap->m_name = "swap";
	swapmap->m_limit = (struct mapent *)&swapmap[nswapmap];
	dmmin = getword(nl[SDMMIN].n_value);
	dmmax = getword(nl[SDMMAX].n_value);
	nswap = 0;
	for (sw = swdevt; sw < &swdevt[nswdev]; sw++)
		if (sw->sw_freed)
			nswap += sw->sw_nblks;
	free = 0;
	for (me = (struct mapent *)(swapmap+1);
	    me < (struct mapent *)&swapmap[nswapmap]; me++)
		free += me->m_size;
	tused = 0;
	for (xp = xtext; xp < &xtext[ntext]; xp++)
		if (xp->x_vptr!=NULL) {
			tused += ctod(clrnd(xp->x_size));
			if (xp->x_flag & XPAGV)
				tused += ctod(clrnd(ctopt(xp->x_size)));
		}
	used = tused;
	waste = 0;
	for (pp = proc; pp < &proc[nproc]; pp++) {
		if (pp->p_stat == 0 || pp->p_stat == SZOMB)
			continue;
		if (pp->p_flag & SSYS)
			continue;
		db = ctod(pp->p_dsize), sb = up(db);
		used += sb;
		waste += sb - db;
		db = ctod(pp->p_ssize), sb = up(db);
		used += sb;
		waste += sb - db;
		if ((pp->p_flag&SLOAD) == 0)
			used += ctod(vusize(pp));
	}
	if (totflg) {
#define	btok(x)	((x) / (1024 / DEV_BSIZE))
		printf("%3d/%3d 00k swap\n",
		    btok(used/100), btok((used+free)/100));
		return;
	}
	printf("%dk used (%dk text), %dk free, %dk wasted, %dk missing\n",
	    btok(used), btok(tused), btok(free), btok(waste),
/* a dmmax/2 block goes to argmap */
	    btok(nswap - dmmax/2 - (used + free)));
	printf("avail: ");
	for (i = dmmax; i >= dmmin; i /= 2) {
		j = 0;
		while (rmalloc(swapmap, i) != 0)
			j++;
		if (j) printf("%d*%dk ", j, btok(i));
	}
	free = 0;
	for (me = (struct mapent *)(swapmap+1);
	    me < (struct mapent *)&swapmap[nswapmap]; me++)
		free += me->m_size;
	printf("%d*1k\n", btok(free));
}

up(size)
	register int size;
{
	register int i, block;

	i = 0;
	block = dmmin;
	while (i < size) {
		i += block;
		if (block < dmmax)
			block *= 2;
	}
	return (i);
}

/*
 * Compute number of pages to be allocated to the u. area
 * and data and stack area page tables, which are stored on the
 * disk immediately after the u. area.
 */
vusize(p)
	register struct proc *p;
{
	register int tsz = p->p_tsize / NPTEPG;

	/*
	 * We do not need page table space on the disk for page
	 * table pages wholly containing text. 
	 */
	return (clrnd(UPAGES +
	    clrnd(ctopt(p->p_tsize+p->p_dsize+p->p_ssize+UPAGES)) - tsz));
}

/*
 * Allocate 'size' units from the given
 * map. Return the base of the allocated space.
 * In a map, the addresses are increasing and the
 * list is terminated by a 0 size.
 *
 * Algorithm is first-fit.
 *
 * This routine knows about the interleaving of the swapmap
 * and handles that.
 */
long
rmalloc(mp, size)
	register struct map *mp;
	long size;
{
	register struct mapent *ep = (struct mapent *)(mp+1);
	register int addr;
	register struct mapent *bp;
	swblk_t first, rest;

	if (size <= 0 || size > dmmax)
		return (0);
	/*
	 * Search for a piece of the resource map which has enough
	 * free space to accomodate the request.
	 */
	for (bp = ep; bp->m_size; bp++) {
		if (bp->m_size >= size) {
			/*
			 * If allocating from swapmap,
			 * then have to respect interleaving
			 * boundaries.
			 */
			if (nswdev > 1 &&
			    (first = dmmax - bp->m_addr%dmmax) < bp->m_size) {
				if (bp->m_size - first < size)
					continue;
				addr = bp->m_addr + first;
				rest = bp->m_size - first - size;
				bp->m_size = first;
				if (rest)
					rmfree(mp, rest, addr+size);
				return (addr);
			}
			/*
			 * Allocate from the map.
			 * If there is no space left of the piece
			 * we allocated from, move the rest of
			 * the pieces to the left.
			 */
			addr = bp->m_addr;
			bp->m_addr += size;
			if ((bp->m_size -= size) == 0) {
				do {
					bp++;
					(bp-1)->m_addr = bp->m_addr;
				} while ((bp-1)->m_size = bp->m_size);
			}
			if (addr % CLSIZE)
				return (0);
			return (addr);
		}
	}
	return (0);
}

/*
 * Free the previously allocated space at addr
 * of size units into the specified map.
 * Sort addr into map and combine on
 * one or both ends if possible.
 */
rmfree(mp, size, addr)
	struct map *mp;
	long size, addr;
{
	struct mapent *firstbp;
	register struct mapent *bp;
	register int t;

	/*
	 * Both address and size must be
	 * positive, or the protocol has broken down.
	 */
	if (addr <= 0 || size <= 0)
		goto badrmfree;
	/*
	 * Locate the piece of the map which starts after the
	 * returned space (or the end of the map).
	 */
	firstbp = bp = (struct mapent *)(mp + 1);
	for (; bp->m_addr <= addr && bp->m_size != 0; bp++)
		continue;
	/*
	 * If the piece on the left abuts us,
	 * then we should combine with it.
	 */
	if (bp > firstbp && (bp-1)->m_addr+(bp-1)->m_size >= addr) {
		/*
		 * Check no overlap (internal error).
		 */
		if ((bp-1)->m_addr+(bp-1)->m_size > addr)
			goto badrmfree;
		/*
		 * Add into piece on the left by increasing its size.
		 */
		(bp-1)->m_size += size;
		/*
		 * If the combined piece abuts the piece on
		 * the right now, compress it in also,
		 * by shifting the remaining pieces of the map over.
		 */
		if (bp->m_addr && addr+size >= bp->m_addr) {
			if (addr+size > bp->m_addr)
				goto badrmfree;
			(bp-1)->m_size += bp->m_size;
			while (bp->m_size) {
				bp++;
				(bp-1)->m_addr = bp->m_addr;
				(bp-1)->m_size = bp->m_size;
			}
		}
		goto done;
	}
	/*
	 * Don't abut on the left, check for abutting on
	 * the right.
	 */
	if (addr+size >= bp->m_addr && bp->m_size) {
		if (addr+size > bp->m_addr)
			goto badrmfree;
		bp->m_addr -= size;
		bp->m_size += size;
		goto done;
	}
	/*
	 * Don't abut at all.  Make a new entry
	 * and check for map overflow.
	 */
	do {
		t = bp->m_addr;
		bp->m_addr = addr;
		addr = t;
		t = bp->m_size;
		bp->m_size = size;
		bp++;
	} while (size = t);
	/*
	 * Segment at bp is to be the delimiter;
	 * If there is not room for it 
	 * then the table is too full
	 * and we must discard something.
	 */
	if (bp+1 > mp->m_limit) {
		/*
		 * Back bp up to last available segment.
		 * which contains a segment already and must
		 * be made into the delimiter.
		 * Discard second to last entry,
		 * since it is presumably smaller than the last
		 * and move the last entry back one.
		 */
		bp--;
		printf("%s: rmap ovflo, lost [%d,%d)\n", mp->m_name,
		    (bp-1)->m_addr, (bp-1)->m_addr+(bp-1)->m_size);
		bp[-1] = bp[0];
		bp[0].m_size = bp[0].m_addr = 0;
	}
done:
	return;
badrmfree:
	printf("bad rmfree\n");
}

#include <varargs.h>

error(va_alist)
	va_dcl
{
	char *fmt;
	va_list ap;
	extern errno;

	fprintf(stderr, "%s: ", Program);
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

	fprintf(stderr, "%s: ", Program);
	va_start(ap);
	fmt = va_arg(ap, char *);
	(void) vfprintf(stderr, fmt, ap);
	va_end(ap);
	fprintf(stderr, ": %s\n", strerror(errno));
}

/*
 * Copyright (c) 1988 University of Utah.
 * Copyright (c) 1990 The Regents of the University of California.
 * All rights reserved.
 *
 * This code is derived from software contributed to Berkeley by
 * the Systems Programming Group of the University of Utah Computer
 * Science Department.
 *
 * %sccs.include.redist.c%
 *
 * from: Utah $Hdr: hpux_compat.c 1.33 89/08/23$
 *
 *	@(#)hpux_compat.c	7.5 (Berkeley) %G%
 */

/*
 * Various HPUX compatibility routines
 */

#ifdef HPUXCOMPAT

#include "param.h"
#include "systm.h"
#include "syscontext.h"
#include "kernel.h"
#include "proc.h"
#include "buf.h"
#include "wait.h"
#include "file.h"
#include "vnode.h"
#include "ioctl.h"
#include "uio.h"
#include "ptrace.h"
#include "stat.h"
#include "syslog.h"
#include "malloc.h"
#include "mount.h"
#include "ipc.h"

#include "machine/cpu.h"
#include "machine/reg.h"
#include "machine/psl.h"
#include "machine/vmparam.h"
#include "hpux.h"
#include "hpux_termio.h"

#ifdef DEBUG
int unimpresponse = 0;
#endif

/* "tick" value for HZ==50 */
int hpuxtick = 1000000 / 50;

/* SYS5 style UTSNAME info */
struct hpuxutsname protoutsname = {
	"4.4bsd", "", "2.0", "B", "9000/3?0", ""
};

/* 6.0 and later style context */
#ifdef FPCOPROC
char hpuxcontext[] =
	"standalone HP-MC68881 HP-MC68020 HP-MC68010 localroot default";
#else
char hpuxcontext[] =
	"standalone HP-MC68020 HP-MC68010 localroot default";
#endif

/* YP domainname */
char	domainname[MAXHOSTNAMELEN] = "unknown";
int	domainnamelen = 7;

#define NERR	79
#define BERR	1000

/* indexed by BSD errno */
short bsdtohpuxerrnomap[NERR] = {
/*00*/	  0,   1,   2,   3,   4,   5,   6,   7,   8,   9,
/*10*/	 10,  45,  12,  13,  14,  15,  16,  17,  18,  19,
/*20*/	 20,  21,  22,  23,  24,  25,  26,  27,  28,  29,
/*30*/	 30,  31,  32,  33,  34, 246, 245, 244, 216, 217,
/*40*/	218, 219, 220, 221, 222, 223, 224, 225, 226, 227,
/*50*/	228, 229, 230, 231, 232, 233, 234, 235, 236, 237,
/*60*/	238, 239, 249, 248, 241, 242, 247,BERR,BERR,BERR,
/*70*/   70,  71,BERR,BERR,BERR,BERR,BERR,  46,BERR
};

notimp(code, nargs)
{
#ifdef DEBUG
	int *argp = u.u_ap;
	extern char *hpuxsyscallnames[];

	printf("HPUX %s(", hpuxsyscallnames[code]);
	if (nargs)
		while (nargs--)
			printf("%x%c", *argp++, nargs? ',' : ')');
	else
		printf(")");
	printf("\n");
	switch (unimpresponse) {
	case 0:
		nosys();
		break;
	case 1:
		u.u_error = EINVAL;
		break;
	}
#else
	nosys();
#endif
	uprintf("HPUX system call %d not implemented\n", code);
}

/*
 * HPUX versions of wait and wait3 actually pass the parameters
 * (status pointer, options, rusage) into the kernel rather than
 * handling it in the C library stub.  We also need to map any
 * termination signal from BSD to HPUX.
 */
hpuxwait3()
{
	struct a {
		int	*status;
		int	options;
		int	rusage;
	} *uap = (struct a *)u.u_ap;

	/* rusage pointer must be zero */
	if (uap->rusage) {
		u.u_error = EINVAL;
		return;
	}
	u.u_ar0[PS] = PSL_ALLCC;
	u.u_ar0[R0] = uap->options;
	u.u_ar0[R1] = uap->rusage;
	hpuxwait();
}

hpuxwait()
{
	int sig, *statp;
	struct a {
		int	*status;
	} *uap = (struct a *)u.u_ap;

	statp = uap->status;	/* owait clobbers first arg */
	owait();
	/*
	 * HP-UX wait always returns EINTR when interrupted by a signal
	 * (well, unless its emulating a BSD process, but we don't bother...)
	 */
	if (u.u_error == ERESTART)
		u.u_error = EINTR;
	if (u.u_error)
		return;
	sig = u.u_r.r_val2 & 0xFF;
	if (sig == WSTOPPED) {
		sig = (u.u_r.r_val2 >> 8) & 0xFF;
		u.u_r.r_val2 = (bsdtohpuxsig(sig) << 8) | WSTOPPED;
	} else if (sig)
		u.u_r.r_val2 = (u.u_r.r_val2 & 0xFF00) |
			bsdtohpuxsig(sig & 0x7F) | (sig & 0x80);
	if (statp)
		if (suword((caddr_t)statp, u.u_r.r_val2))
			u.u_error = EFAULT;
}

hpuxwaitpid()
{
	int sig, *statp;
	struct a {
		int	pid;
		int	*status;
		int	options;
		struct	rusage *rusage;	/* wait4 arg */
	} *uap = (struct a *)u.u_ap;

	uap->rusage = 0;
	wait4();
	/*
	 * HP-UX wait always returns EINTR when interrupted by a signal
	 * (well, unless its emulating a BSD process, but we don't bother...)
	 */
	if (u.u_error == ERESTART)
		u.u_error = EINTR;
	if (u.u_error)
		return;
	sig = u.u_r.r_val2 & 0xFF;
	if (sig == WSTOPPED) {
		sig = (u.u_r.r_val2 >> 8) & 0xFF;
		u.u_r.r_val2 = (bsdtohpuxsig(sig) << 8) | WSTOPPED;
	} else if (sig)
		u.u_r.r_val2 = (u.u_r.r_val2 & 0xFF00) |
			bsdtohpuxsig(sig & 0x7F) | (sig & 0x80);
	if (statp)
		if (suword((caddr_t)statp, u.u_r.r_val2))
			u.u_error = EFAULT;
}

/*
 * Must remap some bits in the mode mask.
 * O_CREAT, O_TRUNC, and O_EXCL must be remapped,
 * O_SYNCIO (0100000) is removed entirely.
 */
hpuxopen(scp)
	register struct syscontext *scp;
{
	struct a {
		char	*fname;
		int	mode;
		int	crtmode;
	} *uap = (struct a *) scp->sc_ap;
	struct nameidata *ndp = &scp->sc_nd;
	int mode;

	mode = uap->mode;
	uap->mode &= ~(HPUXFSYNCIO|HPUXFEXCL|HPUXFTRUNC|HPUXFCREAT);
	if (mode & HPUXFCREAT) {
		/*
		 * simulate the pre-NFS behavior that opening a
		 * file for READ+CREATE ignores the CREATE (unless
		 * EXCL is set in which case we will return the
		 * proper error).
		 */
		if ((mode & HPUXFEXCL) || ((mode-FOPEN) & FWRITE))
			uap->mode |= FCREAT;
	}
	if (mode & HPUXFTRUNC)
		uap->mode |= FTRUNC;
	if (mode & HPUXFEXCL)
		uap->mode |= FEXCL;
	ndp->ni_segflg = UIO_USERSPACE;
	ndp->ni_dirp = uap->fname;
	RETURN (copen(scp, uap->mode-FOPEN, uap->crtmode &~ scp->sc_cmask, ndp,
		&scp->sc_retval1));
}

hpuxfcntl()
{
	register struct a {
		int	fdes;
		int	cmd;
		int	arg;
	} *uap = (struct a *)u.u_ap;
	int mode;

	switch (uap->cmd) {
	case F_SETFL:
		uap->arg &= ~(HPUXFSYNCIO|HPUXFREMOTE|FUSECACHE);
		break;
	case F_GETFL:
	case F_DUPFD:
	case F_GETFD:
	case F_SETFD:
		break;
	default:
		u.u_error = EINVAL;
		return;
	}
	fcntl();
	if (u.u_error == 0 && uap->arg == F_GETFL) {
		mode = u.u_r.r_val1;
		u.u_r.r_val1 &= ~(FCREAT|FTRUNC|FEXCL|FUSECACHE);
		if (mode & FCREAT)
			u.u_r.r_val1 |= HPUXFCREAT;
		if (mode & FTRUNC)
			u.u_r.r_val1 |= HPUXFTRUNC;
		if (mode & FEXCL)
			u.u_r.r_val1 |= HPUXFEXCL;
	}
}

/*
 * Read and write should return a 0 count when an operation
 * on a VNODE would block, not an error.  Sockets appear to
 * return EWOULDBLOCK (at least in 6.2).  This is probably
 * not entirely correct, since the behavior is only defined
 * for pipes and tty type devices.
 */
hpuxread()
{
	struct a {
		int	fd;
	} *uap = (struct a *)u.u_ap;

	read();
	if (u.u_error == EWOULDBLOCK &&
	    u.u_ofile[uap->fd]->f_type == DTYPE_VNODE) {
		u.u_error = 0;
		u.u_r.r_val1 = 0;
	}
}

hpuxwrite()
{
	struct a {
		int	fd;
	} *uap = (struct a *)u.u_ap;

	write();
	if (u.u_error == EWOULDBLOCK &&
	    u.u_ofile[uap->fd]->f_type == DTYPE_VNODE) {
		u.u_error = 0;
		u.u_r.r_val1 = 0;
	}
}

hpuxreadv()
{
	struct a {
		int	fd;
	} *uap = (struct a *)u.u_ap;

	readv();
	if (u.u_error == EWOULDBLOCK &&
	    u.u_ofile[uap->fd]->f_type == DTYPE_VNODE) {
		u.u_error = 0;
		u.u_r.r_val1 = 0;
	}
}

hpuxwritev()
{
	struct a {
		int	fd;
	} *uap = (struct a *)u.u_ap;

	writev();
	if (u.u_error == EWOULDBLOCK &&
	    u.u_ofile[uap->fd]->f_type == DTYPE_VNODE) {
		u.u_error = 0;
		u.u_r.r_val1 = 0;
	}
}

/*
 * 4.3bsd dup allows dup2 to come in on the same syscall entry
 * and hence allows two arguments.  HPUX dup has only one arg.
 */
hpuxdup()
{
	register struct a {
		int	i;
	} *uap = (struct a *)u.u_ap;
	struct file *fp;
	int j;

	if ((unsigned)uap->i >= NOFILE || (fp = u.u_ofile[uap->i]) == NULL) {
		u.u_error = EBADF;
		return;
	}
	u.u_error = ufalloc(0, &j);
	if (u.u_error)
		return;
	u.u_r.r_val1 = j;
	dupit(j, fp, u.u_pofile[uap->i] &~ UF_EXCLOSE);
}

hpuxuname()
{
	register struct a {
		struct hpuxutsname *uts;
		int dev;
		int request;
	} *uap = (struct a *)u.u_ap;
	register int i;

	switch (uap->request) {
	/* uname */
	case 0:
		/* fill in machine type */
		switch (machineid) {
		case HP_320:
			protoutsname.machine[6] = '2';
			break;
		/* includes 318 and 319 */
		case HP_330:
			protoutsname.machine[6] = '3';
			break;
		case HP_340:
			protoutsname.machine[6] = '4';
			break;
		case HP_350:
			protoutsname.machine[6] = '5';
			break;
		case HP_360:
			protoutsname.machine[6] = '6';
			break;
		case HP_370:
			protoutsname.machine[6] = '7';
			break;
		}
		/* copy hostname (sans domain) to nodename */
		for (i = 0; i < 9 && hostname[i] != '.'; i++)
			protoutsname.nodename[i] = hostname[i];
		u.u_error = copyout((caddr_t)&protoutsname, (caddr_t)uap->uts,
				    sizeof(struct hpuxutsname));
		break;
	/* ustat - who cares? */
	case 2:
	default:
		u.u_error = EINVAL;
		break;
	}
}

hpuxstat()
{
	struct a {
		char	*fname;
		struct hpuxstat *hsb;
	} *uap = (struct a *)u.u_ap;

	u.u_error = hpuxstat1(uap->fname, uap->hsb, FOLLOW);
}

hpuxlstat()
{
	struct a {
		char	*fname;
		struct	hpuxstat *hsb;
	} *uap = (struct a *) u.u_ap;

	u.u_error = hpuxstat1(uap->fname, uap->hsb, NOFOLLOW);
}

hpuxfstat()
{
	register struct file *fp;
	register struct a {
		int	fdes;
		struct	hpuxstat *hsb;
	} *uap = (struct a *)u.u_ap;
	struct stat sb;

	if ((unsigned)uap->fdes >= NOFILE ||
	    (fp = u.u_ofile[uap->fdes]) == NULL) {
		u.u_error = EBADF;
		return;
	}
	switch (fp->f_type) {

	case DTYPE_VNODE:
		u.u_error = vn_stat((struct vnode *)fp->f_data, &sb);
		break;

	case DTYPE_SOCKET:
		u.u_error = soo_stat((struct socket *)fp->f_data, &sb);
		break;

	default:
		panic("fstat");
		/*NOTREACHED*/
	}
	/* is this right for sockets?? */
	if (u.u_error == 0)
		u.u_error = bsdtohpuxstat(&sb, uap->hsb);
}

hpuxulimit()
{
	struct a {
		int	cmd;
		long	newlimit;
	} *uap = (struct a *)u.u_ap;
	struct rlimit *limp;

	limp = &u.u_rlimit[RLIMIT_FSIZE];
	switch (uap->cmd) {
	case 2:
		uap->newlimit *= 512;
		if (uap->newlimit > limp->rlim_max &&
		    (u.u_error = suser(u.u_cred, &u.u_acflag)))
			break;
		limp->rlim_cur = limp->rlim_max = uap->newlimit;
		/* else fall into... */

	case 1:
		u.u_r.r_off = limp->rlim_max / 512;
		break;

	case 3:
		limp = &u.u_rlimit[RLIMIT_DATA];
		u.u_r.r_off = ctob(u.u_tsize) + limp->rlim_max;
		break;

	default:
		u.u_error = EINVAL;
		break;
	}
}

/*
 * Map "real time" priorities 0 (high) thru 127 (low) into nice
 * values -16 (high) thru -1 (low).
 */
hpuxrtprio()
{
	register struct a {
		int pid;
		int prio;
	} *uap = (struct a *)u.u_ap;
	struct proc *p;
	int nice;

	if (uap->prio < RTPRIO_MIN && uap->prio > RTPRIO_MAX &&
	    uap->prio != RTPRIO_NOCHG && uap->prio != RTPRIO_RTOFF) {
		u.u_error = EINVAL;
		return;
	}
	if (uap->pid == 0)
		p = u.u_procp;
	else if ((p = pfind(uap->pid)) == 0) {
		u.u_error = ESRCH;
		return;
	}
	nice = p->p_nice;
	if (nice < NZERO)
		u.u_r.r_val1 = (nice + 16) << 3;
	else
		u.u_r.r_val1 = RTPRIO_RTOFF;
	switch (uap->prio) {

	case RTPRIO_NOCHG:
		return;

	case RTPRIO_RTOFF:
		if (nice >= NZERO)
			return;
		nice = NZERO;
		break;

	default:
		nice = (uap->prio >> 3) - 16;
		break;
	}
	donice(p, nice);
	if (u.u_error == EACCES)
		u.u_error = EPERM;
}

/*
 * Kudos to HP folks for using such mnemonic names so I could figure
 * out what this guy does.
 */
hpuxadvise()
{
	struct a {
		int	arg;
	} *uap = (struct a *) u.u_ap;

	switch (uap->arg) {
	case 0:
		u.u_pcb.pcb_flags |= PCB_HPUXMMAP;
		break;
	case 1:
		ICIA();
		break;
	case 2:
		DCIA();
		break;
	default:
		u.u_error = EINVAL;
		break;
	}
}

hpuxptrace()
{
	struct a {
		int	req;
		int	pid;
		int	*addr;
		int	data;
	} *uap = (struct a *)u.u_ap;

	if (uap->req == PT_STEP || uap->req == PT_CONTINUE) {
		if (uap->data) {
			uap->data = hpuxtobsdsig(uap->data);
			if (uap->data == 0)
				uap->data = NSIG;
		}
	}
	ptrace();
}

hpuxgetdomainname()
{
	register struct a {
		char	*domainname;
		u_int	len;
	} *uap = (struct a *)u.u_ap;

	if (uap->len > domainnamelen + 1)
		uap->len = domainnamelen + 1;
	u.u_error = copyout(domainname, uap->domainname, uap->len);
}

hpuxsetdomainname()
{
	register struct a {
		char	*domainname;
		u_int	len;
	} *uap = (struct a *)u.u_ap;

	if (u.u_error = suser(u.u_cred, &u.u_acflag))
		return;
	if (uap->len > sizeof (domainname) - 1) {
		u.u_error = EINVAL;
		return;
	}
	domainnamelen = uap->len;
	u.u_error = copyin(uap->domainname, domainname, uap->len);
	domainname[domainnamelen] = 0;
}

#ifdef SYSVSHM
hpuxshmat()
{
	shmat(u.u_ap);
}

hpuxshmctl()
{
	shmctl(u.u_ap);
}

hpuxshmdt()
{
	shmdt(u.u_ap);
}

hpuxshmget()
{
	shmget(u.u_ap);
}
#endif

/*
 * Fake semaphore routines, just don't return an error.
 * Should be adequate for starbase to run.
 */
hpuxsemctl()
{
	struct a {
		int semid;
		u_int semnum;
		int cmd;
		int arg;
	} *uap = (struct a *)u.u_ap;

	/* XXX: should do something here */
}

hpuxsemget()
{
	struct a {
		key_t key;
		int nsems;
		int semflg;
	} *uap = (struct a *)u.u_ap;

	/* XXX: should do something here */
}

hpuxsemop()
{
	struct a {
		int semid;
		struct sembuf *sops;
		u_int nsops;
	} *uap = (struct a *)u.u_ap;

	/* XXX: should do something here */
}

/* convert from BSD to HPUX errno */
bsdtohpuxerrno(err)
	int err;
{
	if (err < 0 || err >= NERR)
		return(BERR);
	return((int)bsdtohpuxerrnomap[err]);
}

hpuxstat1(fname, hsb, follow)
	char *fname;
	struct hpuxstat *hsb;
	int follow;
{
	register struct nameidata *ndp = &u.u_nd;
	struct stat sb;
	int error;

	ndp->ni_nameiop = LOOKUP | LOCKLEAF | follow;
	ndp->ni_segflg = UIO_USERSPACE;
	ndp->ni_dirp = fname;
	if (error = namei(ndp))
		return (error);
	error = vn_stat(ndp->ni_vp, &sb);
	vput(ndp->ni_vp);
	if (error == 0)
		error = bsdtohpuxstat(&sb, hsb);
	return (error);
}

#include "grf.h"

bsdtohpuxstat(sb, hsb)
	struct stat *sb;
	struct hpuxstat *hsb;
{
	struct hpuxstat ds;

	bzero((caddr_t)&ds, sizeof(ds));
	ds.hst_dev = sb->st_dev;
	ds.hst_ino = (u_long)sb->st_ino;
	ds.hst_mode = sb->st_mode;
	ds.hst_nlink = sb->st_nlink;
	ds.hst_uid = (u_short)sb->st_uid;
	ds.hst_gid = (u_short)sb->st_gid;
#if NGRF > 0
	/* XXX: I don't want to talk about it... */
	if ((sb->st_mode & S_IFMT) == S_IFCHR && major(sb->st_rdev) == 10)
		ds.hst_rdev = grfdevno(sb->st_rdev);
	else
#endif
		ds.hst_rdev = bsdtohpuxdev(sb->st_rdev);
	ds.hst_size = sb->st_size;
	ds.hst_atime = sb->st_atime;
	ds.hst_mtime = sb->st_mtime;
	ds.hst_ctime = sb->st_ctime;
	ds.hst_blksize = sb->st_blksize;
	ds.hst_blocks = sb->st_blocks;
	return(copyout((caddr_t)&ds, (caddr_t)hsb, sizeof(ds)));
}

hpuxtobsdioctl(com)
	int com;
{
	switch (com) {
	case HPUXTIOCSLTC:
		com = TIOCSLTC; break;
	case HPUXTIOCGLTC:
		com = TIOCGLTC; break;
	case HPUXTIOCSPGRP:
		com = TIOCSPGRP; break;
	case HPUXTIOCGPGRP:
		com = TIOCGPGRP; break;
	case HPUXTIOCLBIS:
		com = TIOCLBIS; break;
	case HPUXTIOCLBIC:
		com = TIOCLBIC; break;
	case HPUXTIOCLSET:
		com = TIOCLSET; break;
	case HPUXTIOCLGET:
		com = TIOCLGET; break;
	}
	return(com);
}

/*
 * HPUX ioctl system call.  The differences here are:
 *	IOC_IN also means IOC_VOID if the size portion is zero.
 *	no FIOCLEX/FIONCLEX/FIONBIO/FIOASYNC/FIOGETOWN/FIOSETOWN
 *	the sgttyb struct is 2 bytes longer
 */
hpuxioctl()
{
	register struct file *fp;
	struct a {
		int	fdes;
		int	cmd;
		caddr_t	cmarg;
	} *uap = (struct a *)u.u_ap;
	register int com;
	register u_int size;
	caddr_t memp = 0;
#define STK_PARAMS	128
	char stkbuf[STK_PARAMS];
	caddr_t data = stkbuf;

	com = uap->cmd;

	/* XXX */
	if (com == HPUXTIOCGETP || com == HPUXTIOCSETP) {
		getsettty(uap->fdes, com, uap->cmarg);
		return;
	}

	if ((unsigned)uap->fdes >= NOFILE ||
	    (fp = u.u_ofile[uap->fdes]) == NULL) {
		u.u_error = EBADF;
		return;
	}
	if ((fp->f_flag & (FREAD|FWRITE)) == 0) {
		u.u_error = EBADF;
		return;
	}

	/*
	 * Interpret high order word to find
	 * amount of data to be copied to/from the
	 * user's address space.
	 */
	size = IOCPARM_LEN(com);
	if (size > IOCPARM_MAX) {
		u.u_error = EFAULT;
		return;
	}
	if (size > sizeof (stkbuf)) {
		memp = (caddr_t)malloc((u_long)IOCPARM_MAX, M_IOCTLOPS,
		    M_WAITOK);
		data = memp;
	}
	if (com&IOC_IN) {
		if (size) {
			u.u_error = copyin(uap->cmarg, data, (u_int)size);
			if (u.u_error) {
				if (memp)
					free(memp, M_IOCTLOPS);
				return;
			}
		} else
			*(caddr_t *)data = uap->cmarg;
	} else if ((com&IOC_OUT) && size)
		/*
		 * Zero the buffer on the stack so the user
		 * always gets back something deterministic.
		 */
		bzero(data, size);
	else if (com&IOC_VOID)
		*(caddr_t *)data = uap->cmarg;

	switch (com) {

	case HPUXTIOCCONS:
		*(int *)data = 1;
		u.u_error = (*fp->f_ops->fo_ioctl)(fp, TIOCCONS, data);
		break;

	/* BSD-style job control ioctls */
	case HPUXTIOCLBIS:
	case HPUXTIOCLBIC:
	case HPUXTIOCLSET:
		*(int *)data &= HPUXLTOSTOP;
		if (*(int *)data & HPUXLTOSTOP)
			*(int *)data = LTOSTOP;
		/* fall into */
	case HPUXTIOCLGET:
	case HPUXTIOCSLTC:
	case HPUXTIOCGLTC:
	case HPUXTIOCSPGRP:
	case HPUXTIOCGPGRP:
		u.u_error = (*fp->f_ops->fo_ioctl)(fp, hpuxtobsdioctl(com), data);
		if (u.u_error == 0 && com == HPUXTIOCLGET) {
			*(int *)data &= LTOSTOP;
			if (*(int *)data & LTOSTOP)
				*(int *)data = HPUXLTOSTOP;
		}
		break;

	/* SYS 5 termio */
	case HPUXTCGETA:
	case HPUXTCSETA:
	case HPUXTCSETAW:
	case HPUXTCSETAF:
		u.u_error = hpuxtermio(fp, com, data);
		break;

	default:
		u.u_error = (*fp->f_ops->fo_ioctl)(fp, com, data);
		break;
	}
	/*
	 * Copy any data to user, size was
	 * already set and checked above.
	 */
	if (u.u_error == 0 && (com&IOC_OUT) && size)
		u.u_error = copyout(data, uap->cmarg, (u_int)size);
	if (memp)
		free(memp, M_IOCTLOPS);
}

/*
 * Man page lies, behaviour here is based on observed behaviour.
 */
hpuxgetcontext()
{
	struct a {
		char *buf;
		int len;
	} *uap = (struct a *)u.u_ap;
	int error = 0;
	register int len;

	len = MIN(uap->len, sizeof(hpuxcontext));
	if (len)
		error = copyout(hpuxcontext, uap->buf, (u_int)len);
	if (!error)
		u.u_r.r_val1 = sizeof(hpuxcontext);
	return(error);
}

/*
 * XXX: simple recognition hack to see if we can make grmd work.
 */
hpuxlockf()
{
	struct a {
		int fd;
		int func;
		long size;
	} *uap = (struct a *)u.u_ap;
#ifdef DEBUG
	log(LOG_DEBUG, "%d: lockf(%d, %d, %d)\n",
	    u.u_procp->p_pid, uap->fd, uap->func, uap->size);
#endif
	return(0);
}

/*
 * This is the equivalent of BSD getpgrp but with more restrictions.
 * Note we do not check the real uid or "saved" uid.
 */
hpuxgetpgrp2()
{
	register struct proc *p;
	register struct a {
		int pid;
	} *uap = (struct a *)u.u_ap;

	if (uap->pid == 0)
		uap->pid = u.u_procp->p_pid;
	p = pfind(uap->pid);
	if (p == 0) {
		u.u_error = ESRCH;
		return;
	}
	if (u.u_uid && p->p_uid != u.u_uid && !inferior(p)) {
		u.u_error = EPERM;
		return;
	}
	u.u_r.r_val1 = p->p_pgid;
}

/*
 * This is the equivalent of BSD setpgrp but with more restrictions.
 * Note we do not check the real uid or "saved" uid or pgrp.
 */
hpuxsetpgrp2()
{
	struct a {
		int	pid;
		int	pgrp;
	} *uap = (struct a *)u.u_ap;

	/* empirically determined */
	if (uap->pgrp < 0 || uap->pgrp >= 30000) {
		u.u_error = EINVAL;
		return;
	}
	setpgrp();
}

/*
 * Brutal hack!  Map HPUX u-area offsets into BSD u offsets.
 * No apologies offered, if you don't like it, rewrite it!
 */

#define UOFF(f)		((int)&((struct user *)0)->f)
#define HPUOFF(f)	((int)&((struct hpuxuser *)0)->f)

/* simplified FP structure */
struct bsdfp {
	int save[54];
	int reg[24];
	int ctrl[3];
};

hpuxtobsduoff(off)
	int *off;
{
	struct hpuxfp *hp;
	struct bsdfp *bp;
	register u_int raddr;

	/* u_ar0 field */
	if ((int)off == HPUOFF(hpuxu_ar0))
		return(UOFF(u_ar0));

#ifdef FPCOPROC
	/* 68881 registers from PCB */
	hp = (struct hpuxfp *)HPUOFF(hpuxu_fp);
	bp = (struct bsdfp *)UOFF(u_pcb.pcb_fpregs);
	if (off >= hp->hpfp_ctrl && off < &hp->hpfp_ctrl[3])
		return((int)&bp->ctrl[off - hp->hpfp_ctrl]);
	if (off >= hp->hpfp_reg && off < &hp->hpfp_reg[24])
		return((int)&bp->reg[off - hp->hpfp_reg]);
#endif

	/*
	 * Everything else we recognize comes from the kernel stack,
	 * so we convert off to an absolute address (if not already)
	 * for simplicity.
	 */
	if (off < (int *)ctob(UPAGES))
		off = (int *)((u_int)off + (u_int)&u);

	/*
	 * 68020 registers.
	 * We know that the HPUX registers are in the same order as ours.
	 * The only difference is that their PS is 2 bytes instead of a
	 * padded 4 like ours throwing the alignment off.
	 */
	if (off >= u.u_ar0 && off < &u.u_ar0[18]) {
		/*
		 * PS: return low word and high word of PC as HP-UX would
		 * (e.g. &u.u_ar0[16.5]).
		 */
		if (off == &u.u_ar0[PS])
			raddr = (u_int) &((short *)u.u_ar0)[PS*2+1];
		/*
		 * PC: off will be &u.u_ar0[16.5]
		 */
		else if (off == (int *)&(((short *)u.u_ar0)[PS*2+1]))
			raddr = (u_int) &u.u_ar0[PC];
		/*
		 * D0-D7, A0-A7: easy
		 */
		else
			raddr = (u_int) &u.u_ar0[(int)(off - u.u_ar0)];
		return((int)(raddr - (u_int)&u));
	}

	/* everything else */
	return(-1);
}

/*
 * Kludge up a uarea dump so that HPUX debuggers can find out
 * what they need.  IMPORTANT NOTE: we do not EVEN attempt to
 * convert the entire user struct.
 */
hpuxdumpu(vp, cred)
	struct vnode *vp;
	struct ucred *cred;
{
	int error;
	struct hpuxuser *faku;
	struct bsdfp *bp;
	short *foop;

	faku = (struct hpuxuser *)malloc((u_long)ctob(1), M_TEMP, M_WAITOK);
	/*
	 * Make sure there is no mistake about this
	 * being a real user structure.
	 */
	bzero((caddr_t)faku, ctob(1));
	/*
	 * Fill in the process sizes.
	 */
	faku->hpuxu_tsize = u.u_tsize;
	faku->hpuxu_dsize = u.u_dsize;
	faku->hpuxu_ssize = u.u_ssize;
	/*
	 * Fill in the exec header for CDB.
	 * This was saved back in exec().  As far as I can tell CDB
	 * only uses this information to verify that a particular
	 * core file goes with a particular binary.
	 */
	bcopy((caddr_t)u.u_pcb.pcb_exec,
	      (caddr_t)&faku->hpuxu_exdata, sizeof (struct hpux_exec));
	/*
	 * Adjust user's saved registers (on kernel stack) to reflect
	 * HPUX order.  Note that HPUX saves the SR as 2 bytes not 4
	 * so we have to move it up.
	 */
	faku->hpuxu_ar0 = u.u_ar0;
	foop = (short *) u.u_ar0;
	foop[32] = foop[33];
	foop[33] = foop[34];
	foop[34] = foop[35];
#ifdef FPCOPROC
	/*
	 * Copy 68881 registers from our PCB format to HPUX format
	 */
	bp = (struct bsdfp *) &u.u_pcb.pcb_fpregs;
	bcopy((caddr_t)bp->save, (caddr_t)faku->hpuxu_fp.hpfp_save,
	      sizeof(bp->save));
	bcopy((caddr_t)bp->ctrl, (caddr_t)faku->hpuxu_fp.hpfp_ctrl,
	      sizeof(bp->ctrl));
	bcopy((caddr_t)bp->reg, (caddr_t)faku->hpuxu_fp.hpfp_reg,
	      sizeof(bp->reg));
#endif
	/*
	 * Slay the dragon
	 */
	faku->hpuxu_dragon = -1;
	/*
	 * Dump this artfully constructed page in place of the
	 * user struct page.
	 */
	error = vn_rdwr(UIO_WRITE, vp,
			(caddr_t)faku, ctob(1), (off_t)0,
			UIO_SYSSPACE, IO_NODELOCKED|IO_UNIT, cred, (int *)0);
	/*
	 * Dump the remaining UPAGES-1 pages normally
	 */
	if (!error) 
		error = vn_rdwr(UIO_WRITE, vp, ((caddr_t)&u)+ctob(1),
				ctob(UPAGES-1), (off_t)ctob(1), UIO_SYSSPACE,
				IO_NODELOCKED|IO_UNIT, cred, (int *)0);
	free((caddr_t)faku, M_TEMP);
	return(error);
}

/*
 * The remaining routines are essentially the same as those in kern_xxx.c
 * and vfs_xxx.c as defined under "#ifdef COMPAT".  We replicate them here
 * to avoid HPUXCOMPAT dependencies in those files and to make sure that
 * HP-UX compatibility still works even when COMPAT is not defined.
 */
/* #ifdef COMPAT */

#include "../sys/times.h"

/* from old timeb.h */
struct hpuxtimeb {
	time_t	time;
	u_short	millitm;
	short	timezone;
	short	dstflag;
};

/* ye ole stat structure */
struct	ohpuxstat {
	dev_t	ohst_dev;
	u_short	ohst_ino;
	u_short ohst_mode;
	short  	ohst_nlink;
	short  	ohst_uid;
	short  	ohst_gid;
	dev_t	ohst_rdev;
	int	ohst_size;
	int	ohst_atime;
	int	ohst_mtime;
	int	ohst_ctime;
};

/*
 * Right now the following two routines are the same as the 4.3
 * osetuid/osetgid calls.  Eventually they need to be changed to
 * implement the notion of "saved" ids (whatever that means).
 */
ohpuxsetuid()
{
	register uid;
	register struct a {
		int	uid;
	} *uap = (struct a *)u.u_ap;

	uid = uap->uid;
	if (uid != u.u_procp->p_ruid && uid != u.u_cred->cr_uid &&
	    (u.u_error = suser(u.u_cred, &u.u_acflag)))
		return;
	if (u.u_cred->cr_ref > 1)
		u.u_cred = crcopy(u.u_cred);
	u.u_cred->cr_uid = uid;
	u.u_procp->p_uid = uid;
	u.u_procp->p_ruid = uid;
}

ohpuxsetgid()
{
	register gid;
	register struct a {
		int	gid;
	} *uap = (struct a *)u.u_ap;

	gid = uap->gid;
	if (u.u_procp->p_rgid != gid && u.u_cred->cr_groups[0] != gid &&
	    (u.u_error = suser(u.u_cred, &u.u_acflag)))
		return;
	if (u.u_cred->cr_ref > 1)
		u.u_cred = crcopy(u.u_cred);
	u.u_procp->p_rgid = gid;
	u.u_cred->cr_groups[0] = gid;
}

/*
 * SYS V style setpgrp()
 */
ohpuxsetpgrp()
{
	register struct proc *p = u.u_procp;

	if (p->p_pid != p->p_pgid)
		pgmv(p, p->p_pid, 0);
	u.u_r.r_val1 = p->p_pgid;
}

ohpuxtime()
{
	register struct a {
		long	*tp;
	} *uap = (struct a *)u.u_ap;

	if (uap->tp)
		u.u_error = copyout((caddr_t)&time.tv_sec, (caddr_t)uap->tp,
				    sizeof (long));
	u.u_r.r_time = time.tv_sec;
}

ohpuxstime()
{
	register struct a {
		int	time;
	} *uap = (struct a *)u.u_ap;
	struct timeval tv;
	int s;

	tv.tv_sec = uap->time;
	tv.tv_usec = 0;
	u.u_error = suser(u.u_cred, &u.u_acflag);
	if (u.u_error)
		return;

	/* WHAT DO WE DO ABOUT PENDING REAL-TIME TIMEOUTS??? */
	boottime.tv_sec += tv.tv_sec - time.tv_sec;
	s = splhigh(); time = tv; splx(s);
	resettodr();
}

ohpuxftime()
{
	register struct a {
		struct	hpuxtimeb *tp;
	} *uap;
	struct hpuxtimeb tb;
	int s;

	uap = (struct a *)u.u_ap;
	s = splhigh();
	tb.time = time.tv_sec;
	tb.millitm = time.tv_usec / 1000;
	splx(s);
	tb.timezone = tz.tz_minuteswest;
	tb.dstflag = tz.tz_dsttime;
	u.u_error = copyout((caddr_t)&tb, (caddr_t)uap->tp, sizeof (tb));
}

ohpuxalarm()
{
	register struct a {
		int	deltat;
	} *uap = (struct a *)u.u_ap;
	register struct proc *p = u.u_procp;
	int s = splhigh();

	untimeout(realitexpire, (caddr_t)p);
	timerclear(&p->p_realtimer.it_interval);
	u.u_r.r_val1 = 0;
	if (timerisset(&p->p_realtimer.it_value) &&
	    timercmp(&p->p_realtimer.it_value, &time, >))
		u.u_r.r_val1 = p->p_realtimer.it_value.tv_sec - time.tv_sec;
	if (uap->deltat == 0) {
		timerclear(&p->p_realtimer.it_value);
		splx(s);
		return;
	}
	p->p_realtimer.it_value = time;
	p->p_realtimer.it_value.tv_sec += uap->deltat;
	timeout(realitexpire, (caddr_t)p, hzto(&p->p_realtimer.it_value));
	splx(s);
}

ohpuxnice()
{
	register struct a {
		int	niceness;
	} *uap = (struct a *)u.u_ap;
	register struct proc *p = u.u_procp;

	donice(p, (p->p_nice-NZERO)+uap->niceness);
	u.u_r.r_val1 = p->p_nice - NZERO;
}

ohpuxtimes()
{
	register struct a {
		struct	tms *tmsb;
	} *uap = (struct a *)u.u_ap;
	struct tms atms;

	atms.tms_utime = scale50(&u.u_ru.ru_utime);
	atms.tms_stime = scale50(&u.u_ru.ru_stime);
	atms.tms_cutime = scale50(&u.u_cru.ru_utime);
	atms.tms_cstime = scale50(&u.u_cru.ru_stime);
	u.u_error = copyout((caddr_t)&atms, (caddr_t)uap->tmsb, sizeof (atms));
	if (u.u_error == 0)
		u.u_r.r_time = scale50(&time) - scale50(&boottime);
}

scale50(tvp)
	register struct timeval *tvp;
{
	extern int hpuxtick;

	/*
	 * Doesn't exactly do what the documentation says.
	 * What we really do is return 50th of a second since that
	 * is what HZ is on all bobcats I know of.
	 */
	return ((tvp->tv_sec * 50 + tvp->tv_usec / hpuxtick));
}

/*
 * Set IUPD and IACC times on file.
 * Can't set ICHG.
 */
ohpuxutime()
{
	register struct a {
		char	*fname;
		time_t	*tptr;
	} *uap = (struct a *)u.u_ap;
	struct vattr vattr;
	time_t tv[2];
	register struct vnode *vp;
	register struct nameidata *ndp = &u.u_nd;

	if (uap->tptr) {
		u.u_error =
			copyin((caddr_t)uap->tptr, (caddr_t)tv, sizeof (tv));
		if (u.u_error)
			return;
	} else
		tv[0] = tv[1] = time.tv_sec;
	ndp->ni_nameiop = LOOKUP | FOLLOW | LOCKLEAF;
	ndp->ni_segflg = UIO_USERSPACE;
	ndp->ni_dirp = uap->fname;
	vattr_null(&vattr);
	vattr.va_atime.tv_sec = tv[0];
	vattr.va_atime.tv_usec = 0;
	vattr.va_mtime.tv_sec = tv[1];
	vattr.va_mtime.tv_usec = 0;
	if (u.u_error = namei(ndp))
		return;
	vp = ndp->ni_vp;
	if (vp->v_mount->mnt_flag & MNT_RDONLY)
		u.u_error = EROFS;
	else
		u.u_error = VOP_SETATTR(vp, &vattr, ndp->ni_cred);
	vput(vp);
}

ohpuxpause()
{

	(void) tsleep((caddr_t)&u, PPAUSE | PCATCH, "pause", 0);
	/* always return EINTR rather than ERESTART... */
	RETURN (EINTR);
}

/*
 * The old fstat system call.
 */
ohpuxfstat()
{
	register struct a {
		int	fd;
		struct ohpuxstat *sb;
	} *uap = (struct a *)u.u_ap;
	struct file *fp;
	extern struct file *getinode();

	if ((unsigned)uap->fd >= NOFILE || (fp = u.u_ofile[uap->fd]) == NULL) {
		u.u_error = EBADF;
		return;
	}
	if (fp->f_type != DTYPE_VNODE) {
		u.u_error = EINVAL;
		return;
	}
	u.u_error = ohpuxstat1((struct vnode *)fp->f_data, uap->sb);
}

/*
 * Old stat system call.  This version follows links.
 */
ohpuxstat()
{
	register struct a {
		char	*fname;
		struct ohpuxstat *sb;
	} *uap = (struct a *)u.u_ap;
	register struct nameidata *ndp = &u.u_nd;

	ndp->ni_nameiop = LOOKUP | LOCKLEAF | FOLLOW;
	ndp->ni_segflg = UIO_USERSPACE;
	ndp->ni_dirp = uap->fname;
	if (u.u_error = namei(ndp))
		return;
	u.u_error = ohpuxstat1(ndp->ni_vp, uap->sb);
	vput(ndp->ni_vp);
}

int
ohpuxstat1(vp, ub)
	register struct vnode *vp;
	struct ohpuxstat *ub;
{
	struct ohpuxstat ds;
	struct vattr vattr;
	register int error;

	error = VOP_GETATTR(vp, &vattr, u.u_cred);
	if (error)
		return(error);
	/*
	 * Copy from inode table
	 */
	ds.ohst_dev = vattr.va_fsid;
	ds.ohst_ino = (short)vattr.va_fileid;
	ds.ohst_mode = (u_short)vattr.va_mode;
	ds.ohst_nlink = vattr.va_nlink;
	ds.ohst_uid = (short)vattr.va_uid;
	ds.ohst_gid = (short)vattr.va_gid;
	ds.ohst_rdev = (dev_t)vattr.va_rdev;
	ds.ohst_size = (int)vattr.va_size;
	ds.ohst_atime = (int)vattr.va_atime.tv_sec;
	ds.ohst_mtime = (int)vattr.va_mtime.tv_sec;
	ds.ohst_ctime = (int)vattr.va_ctime.tv_sec;
	return (copyout((caddr_t)&ds, (caddr_t)ub, sizeof(ds)));
}
/* #endif */

#endif

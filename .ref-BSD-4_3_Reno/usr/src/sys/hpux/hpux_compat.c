/*
 * Copyright (c) 1988 University of Utah.
 * Copyright (c) 1990 The Regents of the University of California.
 * All rights reserved.
 *
 * This code is derived from software contributed to Berkeley by
 * the Systems Programming Group of the University of Utah Computer
 * Science Department.
 *
 * Redistribution is only permitted until one year after the first shipment
 * of 4.4BSD by the Regents.  Otherwise, redistribution and use in source and
 * binary forms are permitted provided that: (1) source distributions retain
 * this entire copyright notice and comment, and (2) distributions including
 * binaries display the following acknowledgement:  This product includes
 * software developed by the University of California, Berkeley and its
 * contributors'' in the documentation or other materials provided with the
 * distribution and in all advertising materials mentioning features or use
 * of this software.  Neither the name of the University nor the names of
 * its contributors may be used to endorse or promote products derived from
 * this software without specific prior written permission.
 * THIS SOFTWARE IS PROVIDED AS IS'' AND WITHOUT ANY EXPRESS OR IMPLIED
 * WARRANTIES, INCLUDING, WITHOUT LIMITATION, THE IMPLIED WARRANTIES OF
 * MERCHANTABILITY AND FITNESS FOR A PARTICULAR PURPOSE.
 *
 * from: Utah $Hdr: hpux_compat.c 1.33 89/08/23$
 *
 *	@(#)hpux_compat.c	7.9 (Berkeley) 6/28/90
 */

/*
 * Various HPUX compatibility routines
 */

#ifdef HPUXCOMPAT

#include "param.h"
#include "systm.h"
#include "user.h"
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

notimp(p, uap, retval, code, nargs)
	struct proc *p;
	int *uap, *retval;
	int code, nargs;
{
	int error = 0;
#ifdef DEBUG
	register int *argp = uap;
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
		error = nosys(p, uap, retval);
		break;
	case 1:
		error = EINVAL;
		break;
	}
#else
	error = nosys(p, uap, retval);
#endif
	uprintf("HP-UX system call %d not implemented\n", code);
	return (error);
}

/*
 * HPUX versions of wait and wait3 actually pass the parameters
 * (status pointer, options, rusage) into the kernel rather than
 * handling it in the C library stub.  We also need to map any
 * termination signal from BSD to HPUX.
 */
hpuxwait3(p, uap, retval)
	struct proc *p;
	struct args {
		int	*status;
		int	options;
		int	rusage;
	} *uap;
	int *retval;
{
	/* rusage pointer must be zero */
	if (uap->rusage)
		return (EINVAL);
	u.u_ar0[PS] = PSL_ALLCC;
	u.u_ar0[R0] = uap->options;
	u.u_ar0[R1] = uap->rusage;
	return (hpuxwait(p, uap, retval));
}

hpuxwait(p, uap, retval)
	struct proc *p;
	struct args {
		int	*status;
	} *uap;
	int *retval;
{
	int sig, *statp, error;

	statp = uap->status;	/* owait clobbers first arg */
	error = owait(p, uap, retval);
	/*
	 * HP-UX wait always returns EINTR when interrupted by a signal
	 * (well, unless its emulating a BSD process, but we don't bother...)
	 */
	if (error == ERESTART)
		error = EINTR;
	if (error)
		return (error);
	sig = retval[1] & 0xFF;
	if (sig == WSTOPPED) {
		sig = (retval[1] >> 8) & 0xFF;
		retval[1] = (bsdtohpuxsig(sig) << 8) | WSTOPPED;
	} else if (sig)
		retval[1] = (retval[1] & 0xFF00) |
			bsdtohpuxsig(sig & 0x7F) | (sig & 0x80);
	if (statp)
		if (suword((caddr_t)statp, retval[1]))
			error = EFAULT;
	return (error);
}

hpuxwaitpid(p, uap, retval)
	struct proc *p;
	struct args {
		int	pid;
		int	*status;
		int	options;
		struct	rusage *rusage;	/* wait4 arg */
	} *uap;
	int *retval;
{
	int sig, *statp, error;

	uap->rusage = 0;
	error = wait4(p, uap, retval);
	/*
	 * HP-UX wait always returns EINTR when interrupted by a signal
	 * (well, unless its emulating a BSD process, but we don't bother...)
	 */
	if (error == ERESTART)
		error = EINTR;
	if (error)
		return (error);
	sig = retval[1] & 0xFF;
	if (sig == WSTOPPED) {
		sig = (retval[1] >> 8) & 0xFF;
		retval[1] = (bsdtohpuxsig(sig) << 8) | WSTOPPED;
	} else if (sig)
		retval[1] = (retval[1] & 0xFF00) |
			bsdtohpuxsig(sig & 0x7F) | (sig & 0x80);
	if (statp)
		if (suword((caddr_t)statp, retval[1]))
			error = EFAULT;
	return (error);
}

/*
 * Must remap some bits in the mode mask.
 * O_CREAT, O_TRUNC, and O_EXCL must be remapped,
 * O_SYNCIO (0100000) is removed entirely.
 */
hpuxopen(p, uap, retval)
	struct proc *p;
	register struct args {
		char	*fname;
		int	mode;
		int	crtmode;
	} *uap;
	int *retval;
{
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
	return (open(p, uap, retval));
}

hpuxfcntl(p, uap, retval)
	struct proc *p;
	register struct args {
		int	fdes;
		int	cmd;
		int	arg;
	} *uap;
	int *retval;
{
	int mode, error;

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
		return (EINVAL);
	}
	error = fcntl(p, uap, retval);
	if (error == 0 && uap->arg == F_GETFL) {
		mode = *retval;
		*retval &= ~(FCREAT|FTRUNC|FEXCL|FUSECACHE);
		if (mode & FCREAT)
			*retval |= HPUXFCREAT;
		if (mode & FTRUNC)
			*retval |= HPUXFTRUNC;
		if (mode & FEXCL)
			*retval |= HPUXFEXCL;
	}
	return (error);
}

/*
 * Read and write should return a 0 count when an operation
 * on a VNODE would block, not an error.  Sockets appear to
 * return EWOULDBLOCK (at least in 6.2).  This is probably
 * not entirely correct, since the behavior is only defined
 * for pipes and tty type devices.
 */
hpuxread(p, uap, retval)
	struct proc *p;
	struct args {
		int	fd;
	} *uap;
	int *retval;
{
	int error;

	error = read(p, uap, retval);
	if (error == EWOULDBLOCK &&
	    u.u_ofile[uap->fd]->f_type == DTYPE_VNODE) {
		error = 0;
		*retval = 0;
	}
	return (error);
}

hpuxwrite(p, uap, retval)
	struct proc *p;
	struct args {
		int	fd;
	} *uap;
	int *retval;
{
	int error;

	error = write(p, uap, retval);
	if (error == EWOULDBLOCK &&
	    u.u_ofile[uap->fd]->f_type == DTYPE_VNODE) {
		error = 0;
		*retval = 0;
	}
	return (error);
}

hpuxreadv(p, uap, retval)
	struct proc *p;
	struct args {
		int	fd;
	} *uap;
	int *retval;
{
	int error;

	error = readv(p, uap, retval);
	if (error == EWOULDBLOCK &&
	    u.u_ofile[uap->fd]->f_type == DTYPE_VNODE) {
		error = 0;
		*retval = 0;
	}
	return (error);
}

hpuxwritev(p, uap, retval)
	struct proc *p;
	struct args {
		int	fd;
	} *uap;
	int *retval;
{
	int error;

	error = writev(p, uap, retval);
	if (error == EWOULDBLOCK &&
	    u.u_ofile[uap->fd]->f_type == DTYPE_VNODE) {
		error = 0;
		*retval = 0;
	}
	return (error);
}

/*
 * 4.3bsd dup allows dup2 to come in on the same syscall entry
 * and hence allows two arguments.  HPUX dup has only one arg.
 */
hpuxdup(p, uap, retval)
	struct proc *p;
	register struct args {
		int	i;
	} *uap;
	int *retval;
{
	struct file *fp;
	int fd, error;

	if ((unsigned)uap->i >= NOFILE || (fp = u.u_ofile[uap->i]) == NULL)
		return (EBADF);
	if (error = ufalloc(0, &fd))
		return (error);
	*retval = fd;
	u.u_ofile[fd] = fp;
	u.u_pofile[fd] = u.u_pofile[uap->i] &~ UF_EXCLOSE;
	fp->f_count++;
	if (fd > u.u_lastfile)
		u.u_lastfile = fd;
	return (0);
}

hpuxuname(p, uap, retval)
	struct proc *p;
	register struct args {
		struct hpuxutsname *uts;
		int dev;
		int request;
	} *uap;
	int *retval;
{
	register int i;
	int error;

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
		/* includes 345 */
		case HP_375:
			protoutsname.machine[6] = '7';
			protoutsname.machine[7] = '5';
			break;
		}
		/* copy hostname (sans domain) to nodename */
		for (i = 0; i < 9 && hostname[i] != '.'; i++)
			protoutsname.nodename[i] = hostname[i];
		error = copyout((caddr_t)&protoutsname, (caddr_t)uap->uts,
				sizeof(struct hpuxutsname));
		break;
	/* ustat - who cares? */
	case 2:
	default:
		error = EINVAL;
		break;
	}
	return (error);
}

hpuxstat(p, uap, retval)
	struct proc *p;
	struct args {
		char	*fname;
		struct hpuxstat *hsb;
	} *uap;
	int *retval;
{
	return (hpuxstat1(uap->fname, uap->hsb, FOLLOW));
}

hpuxlstat(p, uap, retval)
	struct proc *p;
	struct args {
		char	*fname;
		struct hpuxstat *hsb;
	} *uap;
	int *retval;
{
	return (hpuxstat1(uap->fname, uap->hsb, NOFOLLOW));
}

hpuxfstat(p, uap, retval)
	struct proc *p;
	register struct args {
		int	fdes;
		struct	hpuxstat *hsb;
	} *uap;
	int *retval;
{
	register struct file *fp;
	struct stat sb;
	int error;

	if ((unsigned)uap->fdes >= NOFILE ||
	    (fp = u.u_ofile[uap->fdes]) == NULL)
		return (EBADF);

	switch (fp->f_type) {

	case DTYPE_VNODE:
		error = vn_stat((struct vnode *)fp->f_data, &sb);
		break;

	case DTYPE_SOCKET:
		error = soo_stat((struct socket *)fp->f_data, &sb);
		break;

	default:
		panic("fstat");
		/*NOTREACHED*/
	}
	/* is this right for sockets?? */
	if (error == 0)
		error = bsdtohpuxstat(&sb, uap->hsb);
	return (error);
}

hpuxulimit(p, uap, retval)
	struct proc *p;
	register struct args {
		int	cmd;
		long	newlimit;
	} *uap;
	off_t *retval;
{
	struct rlimit *limp;
	int error = 0;

	limp = &u.u_rlimit[RLIMIT_FSIZE];
	switch (uap->cmd) {
	case 2:
		uap->newlimit *= 512;
		if (uap->newlimit > limp->rlim_max &&
		    (error = suser(u.u_cred, &u.u_acflag)))
			break;
		limp->rlim_cur = limp->rlim_max = uap->newlimit;
		/* else fall into... */

	case 1:
		*retval = limp->rlim_max / 512;		/* XXX */
		break;

	case 3:
		limp = &u.u_rlimit[RLIMIT_DATA];
		*retval = ctob(u.u_tsize) + limp->rlim_max;	/* XXX */
		break;

	default:
		error = EINVAL;
		break;
	}
	return (error);
}

/*
 * Map "real time" priorities 0 (high) thru 127 (low) into nice
 * values -16 (high) thru -1 (low).
 */
hpuxrtprio(cp, uap, retval)
	struct proc *cp;
	register struct args {
		int pid;
		int prio;
	} *uap;
	int *retval;
{
	struct proc *p;
	int nice, error;

	if (uap->prio < RTPRIO_MIN && uap->prio > RTPRIO_MAX &&
	    uap->prio != RTPRIO_NOCHG && uap->prio != RTPRIO_RTOFF)
		return (EINVAL);
	if (uap->pid == 0)
		p = cp;
	else if ((p = pfind(uap->pid)) == 0)
		return (ESRCH);
	nice = p->p_nice;
	if (nice < NZERO)
		*retval = (nice + 16) << 3;
	else
		*retval = RTPRIO_RTOFF;
	switch (uap->prio) {

	case RTPRIO_NOCHG:
		return (0);

	case RTPRIO_RTOFF:
		if (nice >= NZERO)
			return (0);
		nice = NZERO;
		break;

	default:
		nice = (uap->prio >> 3) - 16;
		break;
	}
	error = donice(cp, p, nice);
	if (error == EACCES)
		error = EPERM;
	return (error);
}

hpuxadvise(p, uap, retval)
	struct proc *p;
	struct args {
		int	arg;
	} *uap;
	int *retval;
{
	int error = 0;

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
		error = EINVAL;
		break;
	}
	return (error);
}

hpuxptrace(p, uap, retval)
	struct proc *p;
	struct args {
		int	req;
		int	pid;
		int	*addr;
		int	data;
	} *uap;
	int *retval;
{
	int error;

	if (uap->req == PT_STEP || uap->req == PT_CONTINUE) {
		if (uap->data) {
			uap->data = hpuxtobsdsig(uap->data);
			if (uap->data == 0)
				uap->data = NSIG;
		}
	}
	error = ptrace(p, uap, retval);
	return (error);
}

hpuxgetdomainname(p, uap, retval)
	struct proc *p;
	register struct args {
		char	*domainname;
		u_int	len;
	} *uap;
	int *retval;
{
	if (uap->len > domainnamelen + 1)
		uap->len = domainnamelen + 1;
	return (copyout(domainname, uap->domainname, uap->len));
}

hpuxsetdomainname(p, uap, retval)
	struct proc *p;
	register struct args {
		char	*domainname;
		u_int	len;
	} *uap;
	int *retval;
{
	int error;

	if (error = suser(u.u_cred, &u.u_acflag))
		return (error);
	if (uap->len > sizeof (domainname) - 1)
		return (EINVAL);
	domainnamelen = uap->len;
	error = copyin(uap->domainname, domainname, uap->len);
	domainname[domainnamelen] = 0;
	return (error);
}

#ifdef SYSVSHM
hpuxshmat(p, uap, retval)
	struct proc *p;
	int *uap, *retval;
{
	return (shmat(p, uap, retval));
}

hpuxshmctl(p, uap, retval)
	struct proc *p;
	int *uap, *retval;
{
	return (shmctl(p, uap, retval));
}

hpuxshmdt(p, uap, retval)
	struct proc *p;
	int *uap, *retval;
{
	return (shmdt(p, uap, retval));
}

hpuxshmget(p, uap, retval)
	struct proc *p;
	int *uap, *retval;
{
	return (shmget(p, uap, retval));
}
#endif

/*
 * Fake semaphore routines, just don't return an error.
 * Should be adequate for starbase to run.
 */
hpuxsemctl(p, uap, retval)
	struct proc *p;
	struct args {
		int semid;
		u_int semnum;
		int cmd;
		int arg;
	} *uap;
	int *retval;
{
	/* XXX: should do something here */
	return (0);
}

hpuxsemget(p, uap, retval)
	struct proc *p;
	struct args {
		key_t key;
		int nsems;
		int semflg;
	} *uap;
	int *retval;
{
	/* XXX: should do something here */
	return (0);
}

hpuxsemop(p, uap, retval)
	struct proc *p;
	struct args {
		int semid;
		struct sembuf *sops;
		u_int nsops;
	} *uap;
	int *retval;
{
	/* XXX: should do something here */
	return (0);
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
hpuxioctl(p, uap, retval)
	struct proc *p;
	register struct args {
		int	fdes;
		int	cmd;
		caddr_t	cmarg;
	} *uap;
	int *retval;
{
	register struct file *fp;
	register int com, error;
	register u_int size;
	caddr_t memp = 0;
#define STK_PARAMS	128
	char stkbuf[STK_PARAMS];
	caddr_t data = stkbuf;

	com = uap->cmd;

	/* XXX */
	if (com == HPUXTIOCGETP || com == HPUXTIOCSETP)
		return (getsettty(uap->fdes, com, uap->cmarg));

	if ((unsigned)uap->fdes >= NOFILE ||
	    (fp = u.u_ofile[uap->fdes]) == NULL)
		return (EBADF);
	if ((fp->f_flag & (FREAD|FWRITE)) == 0)
		return (EBADF);

	/*
	 * Interpret high order word to find
	 * amount of data to be copied to/from the
	 * user's address space.
	 */
	size = IOCPARM_LEN(com);
	if (size > IOCPARM_MAX)
		return (ENOTTY);
	if (size > sizeof (stkbuf)) {
		memp = (caddr_t)malloc((u_long)size, M_IOCTLOPS, M_WAITOK);
		data = memp;
	}
	if (com&IOC_IN) {
		if (size) {
			error = copyin(uap->cmarg, data, (u_int)size);
			if (error) {
				if (memp)
					free(memp, M_IOCTLOPS);
				return (error);
			}
		} else
			*(caddr_t *)data = uap->cmarg;
	} else if ((com&IOC_OUT) && size)
		/*
		 * Zero the buffer so the user always
		 * gets back something deterministic.
		 */
		bzero(data, size);
	else if (com&IOC_VOID)
		*(caddr_t *)data = uap->cmarg;

	switch (com) {

	case HPUXTIOCCONS:
		*(int *)data = 1;
		error = (*fp->f_ops->fo_ioctl)(fp, TIOCCONS, data);
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
		error = (*fp->f_ops->fo_ioctl)(fp, hpuxtobsdioctl(com), data);
		if (error == 0 && com == HPUXTIOCLGET) {
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
		error = hpuxtermio(fp, com, data);
		break;

	default:
		error = (*fp->f_ops->fo_ioctl)(fp, com, data);
		break;
	}
	/*
	 * Copy any data to user, size was
	 * already set and checked above.
	 */
	if (error == 0 && (com&IOC_OUT) && size)
		error = copyout(data, uap->cmarg, (u_int)size);
	if (memp)
		free(memp, M_IOCTLOPS);
	return (error);
}

/*
 * Man page lies, behaviour here is based on observed behaviour.
 */
hpuxgetcontext(p, uap, retval)
	struct proc *p;
	struct args {
		char *buf;
		int len;
	} *uap;
	int *retval;
{
	int error = 0;
	register int len;

	len = MIN(uap->len, sizeof(hpuxcontext));
	if (len)
		error = copyout(hpuxcontext, uap->buf, (u_int)len);
	if (error == 0)
		*retval = sizeof(hpuxcontext);
	return (error);
}

/*
 * XXX: simple recognition hack to see if we can make grmd work.
 */
hpuxlockf(p, uap, retval)
	struct proc *p;
	struct args {
		int fd;
		int func;
		long size;
	} *uap;
	int *retval;
{
#ifdef DEBUG
	log(LOG_DEBUG, "%d: lockf(%d, %d, %d)\n",
	    p->p_pid, uap->fd, uap->func, uap->size);
#endif
	return (0);
}

/*
 * This is the equivalent of BSD getpgrp but with more restrictions.
 * Note we do not check the real uid or "saved" uid.
 */
hpuxgetpgrp2(cp, uap, retval)
	struct proc *cp;
	register struct args {
		int pid;
	} *uap;
	int *retval;
{
	register struct proc *p;

	if (uap->pid == 0)
		uap->pid = cp->p_pid;
	p = pfind(uap->pid);
	if (p == 0)
		return (ESRCH);
	if (u.u_uid && p->p_uid != u.u_uid && !inferior(p))
		return (EPERM);
	*retval = p->p_pgid;
	return (0);
}

/*
 * This is the equivalent of BSD setpgrp but with more restrictions.
 * Note we do not check the real uid or "saved" uid or pgrp.
 */
hpuxsetpgrp2(p, uap, retval)
	struct proc *p;
	struct args {
		int	pid;
		int	pgrp;
	} *uap;
	int *retval;
{
	/* empirically determined */
	if (uap->pgrp < 0 || uap->pgrp >= 30000)
		return (EINVAL);
	return (setpgrp(p, uap, retval));
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
 * SYS V style setpgrp()
 */
ohpuxsetpgrp(p, uap, retval)
	register struct proc *p;
	int *uap, *retval;
{
	if (p->p_pid != p->p_pgid)
		pgmv(p, p->p_pid, 0);
	*retval = p->p_pgid;
}

ohpuxtime(p, uap, retval)
	struct proc *p;
	register struct args {
		long	*tp;
	} *uap;
	time_t *retval;
{
	int error;

	if (uap->tp)
		error = copyout((caddr_t)&time.tv_sec, (caddr_t)uap->tp,
				sizeof (long));
	*retval = time.tv_sec;		/* XXX */
	return (error);
}

ohpuxstime(p, uap, retval)
	struct proc *p;
	register struct args {
		int	time;
	} *uap;
	int *retval;
{
	struct timeval tv;
	int s, error;

	tv.tv_sec = uap->time;
	tv.tv_usec = 0;
	if (error = suser(u.u_cred, &u.u_acflag))
		return (error);

	/* WHAT DO WE DO ABOUT PENDING REAL-TIME TIMEOUTS??? */
	boottime.tv_sec += tv.tv_sec - time.tv_sec;
	s = splhigh(); time = tv; splx(s);
	resettodr();
	return (0);
}

ohpuxftime(p, uap, retval)
	struct proc *p;
	register struct args {
		struct	hpuxtimeb *tp;
	} *uap;
	int *retval;
{
	struct hpuxtimeb tb;
	int s;

	s = splhigh();
	tb.time = time.tv_sec;
	tb.millitm = time.tv_usec / 1000;
	splx(s);
	tb.timezone = tz.tz_minuteswest;
	tb.dstflag = tz.tz_dsttime;
	return (copyout((caddr_t)&tb, (caddr_t)uap->tp, sizeof (tb)));
}

ohpuxalarm(p, uap, retval)
	register struct proc *p;
	register struct args {
		int	deltat;
	} *uap;
	int *retval;
{
	int s = splhigh();

	untimeout(realitexpire, (caddr_t)p);
	timerclear(&p->p_realtimer.it_interval);
	*retval = 0;
	if (timerisset(&p->p_realtimer.it_value) &&
	    timercmp(&p->p_realtimer.it_value, &time, >))
		*retval = p->p_realtimer.it_value.tv_sec - time.tv_sec;
	if (uap->deltat == 0) {
		timerclear(&p->p_realtimer.it_value);
		splx(s);
		return (0);
	}
	p->p_realtimer.it_value = time;
	p->p_realtimer.it_value.tv_sec += uap->deltat;
	timeout(realitexpire, (caddr_t)p, hzto(&p->p_realtimer.it_value));
	splx(s);
	return (0);
}

ohpuxnice(p, uap, retval)
	register struct proc *p;
	register struct args {
		int	niceness;
	} *uap;
	int *retval;
{
	int error;

	error = donice(p, p, (p->p_nice-NZERO)+uap->niceness);
	if (error == 0)
		*retval = p->p_nice - NZERO;
	return (error);
}

ohpuxtimes(p, uap, retval)
	struct proc *p;
	register struct args {
		struct	tms *tmsb;
	} *uap;
	time_t *retval;
{
	struct tms atms;
	int error;

	atms.tms_utime = scale50(&u.u_ru.ru_utime);
	atms.tms_stime = scale50(&u.u_ru.ru_stime);
	atms.tms_cutime = scale50(&u.u_cru.ru_utime);
	atms.tms_cstime = scale50(&u.u_cru.ru_stime);
	error = copyout((caddr_t)&atms, (caddr_t)uap->tmsb, sizeof (atms));
	if (error == 0)
		*retval = scale50(&time) - scale50(&boottime); /* XXX */
	return (error);
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
ohpuxutime(p, uap, retval)
	struct proc *p;
	register struct a {
		char	*fname;
		time_t	*tptr;
	} *uap;
	int *retval;
{
	struct vattr vattr;
	time_t tv[2];
	register struct vnode *vp;
	register struct nameidata *ndp = &u.u_nd;
	int error;

	if (uap->tptr) {
		error = copyin((caddr_t)uap->tptr, (caddr_t)tv, sizeof (tv));
		if (error)
			return (error);
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
	if (error = namei(ndp))
		return (error);
	vp = ndp->ni_vp;
	if (vp->v_mount->mnt_flag & MNT_RDONLY)
		error = EROFS;
	else
		error = VOP_SETATTR(vp, &vattr, ndp->ni_cred);
	vput(vp);
	return (error);
}

ohpuxpause(p, uap, retval)
	struct proc *p;
	int *uap, *retval;
{
	(void) tsleep((caddr_t)&u, PPAUSE | PCATCH, "pause", 0);
	/* always return EINTR rather than ERESTART... */
	return (EINTR);
}

/*
 * The old fstat system call.
 */
ohpuxfstat(p, uap, retval)
	struct proc *p;
	register struct args {
		int	fd;
		struct ohpuxstat *sb;
	} *uap;
	int *retval;
{
	struct file *fp;

	if ((unsigned)uap->fd >= NOFILE || (fp = u.u_ofile[uap->fd]) == NULL)
		return (EBADF);
	if (fp->f_type != DTYPE_VNODE)
		return (EINVAL);
	return (ohpuxstat1((struct vnode *)fp->f_data, uap->sb));
}

/*
 * Old stat system call.  This version follows links.
 */
ohpuxstat(p, uap, retval)
	struct proc *p;
	register struct args {
		char	*fname;
		struct ohpuxstat *sb;
	} *uap;
	int *retval;
{
	register struct nameidata *ndp = &u.u_nd;
	int error;

	ndp->ni_nameiop = LOOKUP | LOCKLEAF | FOLLOW;
	ndp->ni_segflg = UIO_USERSPACE;
	ndp->ni_dirp = uap->fname;
	if (error = namei(ndp))
		return (error);
	error = ohpuxstat1(ndp->ni_vp, uap->sb);
	vput(ndp->ni_vp);
	return (error);
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

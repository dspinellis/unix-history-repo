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
 * from: Utah $Hdr: hpux_compat.c 1.55 92/12/26$
 *
 *	@(#)hpux_compat.c	7.33 (Berkeley) %G%
 */

/*
 * Various HP-UX compatibility routines
 */

#ifdef HPUXCOMPAT

#include <sys/param.h>
#include <sys/systm.h>
#include <sys/signalvar.h>
#include <sys/kernel.h>
#include <sys/filedesc.h>
#include <sys/proc.h>
#include <sys/buf.h>
#include <sys/wait.h>
#include <sys/file.h>
#include <sys/namei.h>
#include <sys/vnode.h>
#include <sys/ioctl.h>
#include <sys/ptrace.h>
#include <sys/stat.h>
#include <sys/syslog.h>
#include <sys/malloc.h>
#include <sys/mount.h>
#include <sys/ipc.h>
#include <sys/user.h>
#include <sys/mman.h>

#include <machine/cpu.h>
#include <machine/reg.h>
#include <machine/psl.h>
#include <machine/vmparam.h>
#include <hp/hpux/hpux.h>
#include <hp/hpux/hpux_termio.h>

#ifdef DEBUG
int unimpresponse = 0;
#endif

/* SYS5 style UTSNAME info */
struct hpuxutsname protoutsname = {
	"4.4bsd", "", "0.5", "B", "9000/3?0", ""
};

/* 6.0 and later style context */
#if defined(HP380)
char hpux040context[] =
    "standalone HP-MC68040 HP-MC68881 HP-MC68020 HP-MC68010 localroot default";
#endif
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

	printf("HP-UX %s(", hpuxsyscallnames[code]);
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

struct hpuxexecv_args {
	char	*fname;
	char	**argp;
	char	**envp;
};
hpuxexecv(p, uap, retval)
	struct proc *p;
	struct hpuxexecv_args *uap;
	int *retval;
{
	extern int execve();

	uap->envp = NULL;
	return (execve(p, uap, retval));
}

/*
 * HP-UX versions of wait and wait3 actually pass the parameters
 * (status pointer, options, rusage) into the kernel rather than
 * handling it in the C library stub.  We also need to map any
 * termination signal from BSD to HP-UX.
 */
struct hpuxwait3_args {
	int	*status;
	int	options;
	int	rusage;
};
hpuxwait3(p, uap, retval)
	struct proc *p;
	struct hpuxwait3_args *uap;
	int *retval;
{
	/* rusage pointer must be zero */
	if (uap->rusage)
		return (EINVAL);
	p->p_md.md_regs[PS] = PSL_ALLCC;
	p->p_md.md_regs[R0] = uap->options;
	p->p_md.md_regs[R1] = uap->rusage;
	return (hpuxwait(p, uap, retval));
}

struct hpuxwait_args {
	int	*status;
};
hpuxwait(p, uap, retval)
	struct proc *p;
	struct hpuxwait_args *uap;
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

struct hpuxwaitpid_args {
	int	pid;
	int	*status;
	int	options;
	struct	rusage *rusage;	/* wait4 arg */
};
hpuxwaitpid(p, uap, retval)
	struct proc *p;
	struct hpuxwaitpid_args *uap;
	int *retval;
{
	int rv, sig, xstat, error;

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
	if (uap->status) {
		/*
		 * Wait4 already wrote the status out to user space,
		 * pull it back, change the signal portion, and write
		 * it back out.
		 */
		rv = fuword((caddr_t)uap->status);
		if (WIFSTOPPED(rv)) {
			sig = WSTOPSIG(rv);
			rv = W_STOPCODE(bsdtohpuxsig(sig));
		} else if (WIFSIGNALED(rv)) {
			sig = WTERMSIG(rv);
			xstat = WEXITSTATUS(rv);
			rv = W_EXITCODE(xstat, bsdtohpuxsig(sig)) |
				WCOREDUMP(rv);
		}
		(void)suword((caddr_t)uap->status, rv);
	}
	return (error);
}

/*
 * Must remap some bits in the mode mask.
 * O_CREAT, O_TRUNC, and O_EXCL must be remapped,
 * O_SYNCIO (0100000) is removed entirely.
 */
struct hpuxopen_args {
	char	*fname;
	int	mode;
	int	crtmode;
};
hpuxopen(p, uap, retval)
	struct proc *p;
	register struct hpuxopen_args *uap;
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
		if ((mode & HPUXFEXCL) || (FFLAGS(mode) & FWRITE))
			uap->mode |= O_CREAT;
	}
	if (mode & HPUXFTRUNC)
		uap->mode |= O_TRUNC;
	if (mode & HPUXFEXCL)
		uap->mode |= O_EXCL;
	return (open(p, uap, retval));
}

/*
 * Old creat system call.
 */
struct hpuxcreat_args {
	char	*fname;
	int	fmode;
};
hpuxcreat(p, uap, retval)
	struct proc *p;
	register struct hpuxcreat_args *uap;
	int *retval;
{
	struct nargs {
		char	*fname;
		int	mode;
		int	crtmode;
	} openuap;

	openuap.fname = uap->fname;
	openuap.crtmode = uap->fmode;
	openuap.mode = O_WRONLY | O_CREAT | O_TRUNC;
	return (open(p, &openuap, retval));
}

/* XXX */
#define	UF_FNDELAY_ON	0x20
#define	UF_FIONBIO_ON	0x40
/* XXX */

struct hpuxfcntl_args {
	int	fdes;
	int	cmd;
	int	arg;
};
hpuxfcntl(p, uap, retval)
	struct proc *p;
	register struct hpuxfcntl_args *uap;
	int *retval;
{
	int mode, error;
	char *fp;

	if (uap->cmd == F_GETFL || uap->cmd == F_SETFL) {
		if ((unsigned)uap->fdes >= p->p_fd->fd_nfiles ||
		    p->p_fd->fd_ofiles[uap->fdes] == NULL)
			return (EBADF);
		fp = &p->p_fd->fd_ofileflags[uap->fdes];
	}
	switch (uap->cmd) {
	case F_SETFL:
		if (uap->arg & FNONBLOCK)
			*fp |= UF_FNDELAY_ON;
		else {
			*fp &= ~UF_FNDELAY_ON;
			if (*fp & UF_FIONBIO_ON)
				uap->arg |= FNONBLOCK;
		}
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
	if (error == 0 && uap->cmd == F_GETFL) {
		mode = *retval;
		*retval &= ~(O_CREAT|O_TRUNC|O_EXCL|FUSECACHE);
		if ((mode & FNONBLOCK) && (*fp & UF_FNDELAY_ON) == 0)
			*retval &= ~FNONBLOCK;
		if (mode & O_CREAT)
			*retval |= HPUXFCREAT;
		if (mode & O_TRUNC)
			*retval |= HPUXFTRUNC;
		if (mode & O_EXCL)
			*retval |= HPUXFEXCL;
	}
	return (error);
}

/*
 * Read and write should return a 0 count when an operation
 * on a VNODE would block, not an error.
 *
 * In 6.2 and 6.5 sockets appear to return EWOULDBLOCK.
 * In 7.0 the behavior for sockets depends on whether FNONBLOCK is in effect.
 */
struct hpuxread_args {
	int	fd;
};
hpuxread(p, uap, retval)
	struct proc *p;
	struct hpuxread_args *uap;
	int *retval;
{
	int error;

	error = read(p, uap, retval);
	if (error == EWOULDBLOCK &&
	    (p->p_fd->fd_ofiles[uap->fd]->f_type == DTYPE_VNODE ||
	     p->p_fd->fd_ofileflags[uap->fd] & UF_FNDELAY_ON)) {
		error = 0;
		*retval = 0;
	}
	return (error);
}

struct hpuxwrite_args {
	int	fd;
};
hpuxwrite(p, uap, retval)
	struct proc *p;
	struct hpuxwrite_args *uap;
	int *retval;
{
	int error;

	error = write(p, uap, retval);
	if (error == EWOULDBLOCK &&
	    (p->p_fd->fd_ofiles[uap->fd]->f_type == DTYPE_VNODE ||
	     p->p_fd->fd_ofileflags[uap->fd] & UF_FNDELAY_ON)) {
		error = 0;
		*retval = 0;
	}
	return (error);
}

struct hpuxreadv_args {
	int	fd;
};
hpuxreadv(p, uap, retval)
	struct proc *p;
	struct hpuxreadv_args *uap;
	int *retval;
{
	int error;

	error = readv(p, uap, retval);
	if (error == EWOULDBLOCK &&
	    (p->p_fd->fd_ofiles[uap->fd]->f_type == DTYPE_VNODE ||
	     p->p_fd->fd_ofileflags[uap->fd] & UF_FNDELAY_ON)) {
		error = 0;
		*retval = 0;
	}
	return (error);
}

struct hpuxwritev_args {
	int	fd;
};
hpuxwritev(p, uap, retval)
	struct proc *p;
	struct hpuxwritev_args *uap;
	int *retval;
{
	int error;

	error = writev(p, uap, retval);
	if (error == EWOULDBLOCK &&
	    (p->p_fd->fd_ofiles[uap->fd]->f_type == DTYPE_VNODE ||
	     p->p_fd->fd_ofileflags[uap->fd] & UF_FNDELAY_ON)) {
		error = 0;
		*retval = 0;
	}
	return (error);
}

/*
 * 4.3bsd dup allows dup2 to come in on the same syscall entry
 * and hence allows two arguments.  HP-UX dup has only one arg.
 */
struct hpuxdup_args {
	int	i;
};
hpuxdup(p, uap, retval)
	struct proc *p;
	register struct hpuxdup_args *uap;
	int *retval;
{
	register struct filedesc *fdp = p->p_fd;
	struct file *fp;
	int fd, error;

	if (((unsigned)uap->i) >= fdp->fd_nfiles ||
	    (fp = fdp->fd_ofiles[uap->i]) == NULL)
		return (EBADF);
	if (error = fdalloc(p, 0, &fd))
		return (error);
	fdp->fd_ofiles[fd] = fp;
	fdp->fd_ofileflags[fd] = fdp->fd_ofileflags[uap->i] &~ UF_EXCLOSE;
	fp->f_count++;
	if (fd > fdp->fd_lastfile)
		fdp->fd_lastfile = fd;
	*retval = fd;
	return (0);
}

struct hpuxutssys_args {
	struct hpuxutsname *uts;
	int dev;
	int request;
};
hpuxutssys(p, uap, retval)
	struct proc *p;
	register struct hpuxutssys_args *uap;
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
		/* includes 425 */
		case HP_380:
			protoutsname.machine[6] = '8';
			break;
		case HP_433:
			protoutsname.machine[5] = '4';
			protoutsname.machine[6] = '3';
			protoutsname.machine[7] = '3';
			break;
		}
		/* copy hostname (sans domain) to nodename */
		for (i = 0; i < 8 && hostname[i] != '.'; i++)
			protoutsname.nodename[i] = hostname[i];
		protoutsname.nodename[i] = '\0';
		error = copyout((caddr_t)&protoutsname, (caddr_t)uap->uts,
				sizeof(struct hpuxutsname));
		break;

	/* gethostname */
	case 5:
		/* uap->dev is length */
		if (uap->dev > hostnamelen + 1)
			uap->dev = hostnamelen + 1;
		error = copyout((caddr_t)hostname, (caddr_t)uap->uts,
				uap->dev);
		break;

	case 1:	/* ?? */
	case 2:	/* ustat */
	case 3:	/* ?? */
	case 4:	/* sethostname */
	default:
		error = EINVAL;
		break;
	}
	return (error);
}

struct hpuxsysconf_args {
	int	name;
};
hpuxsysconf(p, uap, retval)
	struct proc *p;
	struct hpuxsysconf_args *uap;
	int *retval;
{
	switch (uap->name) {

	/* open files */
	case HPUX_SYSCONF_OPENMAX:
		*retval = NOFILE;
		break;

	/* architecture */
	case HPUX_SYSCONF_CPUTYPE:
		switch (machineid) {
		case HP_320:
		case HP_330:
		case HP_350:
			*retval = HPUX_SYSCONF_CPUM020;
			break;
		case HP_340:
		case HP_360:
		case HP_370:
		case HP_375:
			*retval = HPUX_SYSCONF_CPUM030;
			break;
		case HP_380:
		case HP_433:
			*retval = HPUX_SYSCONF_CPUM040;
			break;
		}
		break;
	default:
		uprintf("HP-UX sysconf(%d) not implemented\n", uap->name);
		return (EINVAL);
	}
	return (0);
}

struct hpuxstat_args {
	char	*fname;
	struct hpuxstat *hsb;
};
hpuxstat(p, uap, retval)
	struct proc *p;
	struct hpuxstat_args *uap;
	int *retval;
{
	return (hpuxstat1(uap->fname, uap->hsb, FOLLOW, p));
}

struct hpuxlstat_args {
	char	*fname;
	struct hpuxstat *hsb;
};
hpuxlstat(p, uap, retval)
	struct proc *p;
	struct hpuxlstat_args *uap;
	int *retval;
{
	return (hpuxstat1(uap->fname, uap->hsb, NOFOLLOW, p));
}

struct hpuxfstat_args {
	int	fdes;
	struct	hpuxstat *hsb;
};
hpuxfstat(p, uap, retval)
	struct proc *p;
	register struct hpuxfstat_args *uap;
	int *retval;
{
	register struct filedesc *fdp = p->p_fd;
	register struct file *fp;
	struct stat sb;
	int error;

	if (((unsigned)uap->fdes) >= fdp->fd_nfiles ||
	    (fp = fdp->fd_ofiles[uap->fdes]) == NULL)
		return (EBADF);

	switch (fp->f_type) {

	case DTYPE_VNODE:
		error = vn_stat((struct vnode *)fp->f_data, &sb, p);
		break;

	case DTYPE_SOCKET:
		error = soo_stat((struct socket *)fp->f_data, &sb, p);
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

struct hpuxulimit_args {
	int	cmd;
	long	newlimit;
};
hpuxulimit(p, uap, retval)
	struct proc *p;
	register struct hpuxulimit_args *uap;
	long *retval;
{
	struct rlimit *limp;
	int error = 0;

	limp = &p->p_rlimit[RLIMIT_FSIZE];
	switch (uap->cmd) {
	case 2:
		uap->newlimit *= 512;
		if (uap->newlimit > limp->rlim_max &&
		    (error = suser(p->p_ucred, &p->p_acflag)))
			break;
		limp->rlim_cur = limp->rlim_max = uap->newlimit;
		/* else fall into... */

	case 1:
		*retval = limp->rlim_max / 512;
		break;

	case 3:
		limp = &p->p_rlimit[RLIMIT_DATA];
		*retval = ctob(p->p_vmspace->vm_tsize) + limp->rlim_max;
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
struct hpuxrtprio_args {
	int pid;
	int prio;
};
hpuxrtprio(cp, uap, retval)
	struct proc *cp;
	register struct hpuxrtprio_args *uap;
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

struct hpuxadvise_args {
	int	arg;
};
hpuxadvise(p, uap, retval)
	struct proc *p;
	struct hpuxadvise_args *uap;
	int *retval;
{
	int error = 0;

	switch (uap->arg) {
	case 0:
		p->p_md.md_flags |= MDP_HPUXMMAP;
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

struct hpuxptrace_args {
	int	req;
	int	pid;
	int	*addr;
	int	data;
};
hpuxptrace(p, uap, retval)
	struct proc *p;
	struct hpuxptrace_args *uap;
	int *retval;
{
	int error, isps = 0;
	struct proc *cp;

	switch (uap->req) {
	/* map signal */
	case PT_STEP:
	case PT_CONTINUE:
		if (uap->data) {
			uap->data = hpuxtobsdsig(uap->data);
			if (uap->data == 0)
				uap->data = NSIG;
		}
		break;
	/* map u-area offset */
	case PT_READ_U:
	case PT_WRITE_U:
		/*
		 * Big, cheezy hack: hpuxtobsduoff is really intended
		 * to be called in the child context (procxmt) but we
		 * do it here in the parent context to avoid hacks in
		 * the MI sys_process.c file.  This works only because
		 * we can access the child's md_regs pointer and it
		 * has the correct value (the child has already trapped
		 * into the kernel).
		 */
		if ((cp = pfind(uap->pid)) == 0)
			return (ESRCH);
		uap->addr = (int *) hpuxtobsduoff(uap->addr, &isps, cp);

		/*
		 * Since HP-UX PS is only 16-bits in ar0, requests
		 * to write PS actually contain the PS in the high word
		 * and the high half of the PC (the following register)
		 * in the low word.  Move the PS value to where BSD
		 * expects it.
		 */
		if (isps && uap->req == PT_WRITE_U)
			uap->data >>= 16;
		break;
	}
	error = ptrace(p, uap, retval);
	/*
	 * Align PS as HP-UX expects it (see WRITE_U comment above).
	 * Note that we do not return the high part of PC like HP-UX
	 * would, but the HP-UX debuggers don't require it.
	 */
	if (isps && error == 0 && uap->req == PT_READ_U)
		*retval <<= 16;
	return (error);
}

struct hpuxgetdomainname_args {
	char	*domainname;
	u_int	len;
};
hpuxgetdomainname(p, uap, retval)
	struct proc *p;
	register struct hpuxgetdomainname_args *uap;
	int *retval;
{
	if (uap->len > domainnamelen + 1)
		uap->len = domainnamelen + 1;
	return (copyout(domainname, uap->domainname, uap->len));
}

struct hpuxsetdomainname_args {
	char	*domainname;
	u_int	len;
};
hpuxsetdomainname(p, uap, retval)
	struct proc *p;
	register struct hpuxsetdomainname_args *uap;
	int *retval;
{
	int error;

	if (error = suser(p->p_ucred, &p->p_acflag))
		return (error);
	if (uap->len > sizeof (domainname) - 1)
		return (EINVAL);
	domainnamelen = uap->len;
	error = copyin(uap->domainname, domainname, uap->len);
	domainname[domainnamelen] = 0;
	return (error);
}

#ifdef SYSVSHM
#include <sys/shm.h>

hpuxshmat(p, uap, retval)
	struct proc *p;
	int *uap, *retval;
{
	return (shmat(p, uap, retval));
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

/*
 * Handle HP-UX specific commands.
 */
struct hpuxshmctl_args {
	int shmid;
	int cmd;
	caddr_t buf;
};
hpuxshmctl(p, uap, retval)
	struct proc *p;
	struct hpuxshmctl_args *uap;
	int *retval;
{
	register struct shmid_ds *shp;
	register struct ucred *cred = p->p_ucred;
	int error;

	if (error = shmvalid(uap->shmid))
		return (error);
	shp = &shmsegs[uap->shmid % SHMMMNI];
	if (uap->cmd == SHM_LOCK || uap->cmd == SHM_UNLOCK) {
		/* don't really do anything, but make them think we did */
		if (cred->cr_uid && cred->cr_uid != shp->shm_perm.uid &&
		    cred->cr_uid != shp->shm_perm.cuid)
			return (EPERM);
		return (0);
	}
	return (shmctl(p, uap, retval));
}
#endif

/*
 * Fake semaphore routines, just don't return an error.
 * Should be adequate for starbase to run.
 */
struct hpuxsemctl_args {
	int semid;
	u_int semnum;
	int cmd;
	int arg;
};
hpuxsemctl(p, uap, retval)
	struct proc *p;
	struct hpuxsemctl_args *uap;
	int *retval;
{
	/* XXX: should do something here */
	return (0);
}

struct hpuxsemget_args {
	key_t key;
	int nsems;
	int semflg;
};
hpuxsemget(p, uap, retval)
	struct proc *p;
	struct hpuxsemget_args *uap;
	int *retval;
{
	/* XXX: should do something here */
	return (0);
}

struct hpuxsemop_args {
	int semid;
	struct sembuf *sops;
	u_int nsops;
};
hpuxsemop(p, uap, retval)
	struct proc *p;
	struct hpuxsemop_args *uap;
	int *retval;
{
	/* XXX: should do something here */
	return (0);
}

/*
 * HP-UX mmap() emulation (mainly for shared library support).
 */
struct hpuxmmap_args {
	caddr_t	addr;
	int	len;
	int	prot;
	int	flags;
	int	fd;
	long	pos;
};
hpuxmmap(p, uap, retval)
	struct proc *p;
	struct hpuxmmap_args *uap;
	int *retval;
{
	struct mmap_args {
		caddr_t	addr;
		int	len;
		int	prot;
		int	flags;
		int	fd;
		long	pad;
		off_t	pos;
	} nargs;

	nargs.addr = uap->addr;
	nargs.len = uap->len;
	nargs.prot = uap->prot;
	nargs.flags = uap->flags &
		~(HPUXMAP_FIXED|HPUXMAP_REPLACE|HPUXMAP_ANON);
	if (uap->flags & HPUXMAP_FIXED)
		nargs.flags |= MAP_FIXED;
	if (uap->flags & HPUXMAP_ANON)
		nargs.flags |= MAP_ANON;
	nargs.fd = (nargs.flags & MAP_ANON) ? -1 : uap->fd;
	nargs.pos = uap->pos;
	return (smmap(p, &nargs, retval));
}

/* convert from BSD to HP-UX errno */
bsdtohpuxerrno(err)
	int err;
{
	if (err < 0 || err >= NERR)
		return(BERR);
	return((int)bsdtohpuxerrnomap[err]);
}

hpuxstat1(fname, hsb, follow, p)
	char *fname;
	struct hpuxstat *hsb;
	int follow;
	struct proc *p;
{
	int error;
	struct stat sb;
	struct nameidata nd;

	NDINIT(&nd, LOOKUP, follow | LOCKLEAF, UIO_USERSPACE, fname, p);
	if (error = namei(&nd))
		return (error);
	error = vn_stat(nd.ni_vp, &sb, p);
	vput(nd.ni_vp);
	if (error == 0)
		error = bsdtohpuxstat(&sb, hsb);
	return (error);
}

#include "grf.h"
#if NGRF > 0
#ifdef __STDC__
extern int grfopen(dev_t dev, int oflags, int devtype, struct proc *p);
#else
extern int grfopen();
#endif
#endif

#define	NHIL	1	/* XXX */
#if NHIL > 0
#ifdef __STDC__
extern int hilopen(dev_t dev, int oflags, int devtype, struct proc *p);
#else
extern int hilopen();
#endif
#endif

#include <sys/conf.h>

bsdtohpuxstat(sb, hsb)
	struct stat *sb;
	struct hpuxstat *hsb;
{
	struct hpuxstat ds;

	bzero((caddr_t)&ds, sizeof(ds));
	ds.hst_dev = (u_short)sb->st_dev;
	ds.hst_ino = (u_long)sb->st_ino;
	ds.hst_mode = sb->st_mode;
	ds.hst_nlink = sb->st_nlink;
	ds.hst_uid = (u_short)sb->st_uid;
	ds.hst_gid = (u_short)sb->st_gid;
	ds.hst_rdev = bsdtohpuxdev(sb->st_rdev);

	/* XXX: I don't want to talk about it... */
	if ((sb->st_mode & S_IFMT) == S_IFCHR) {
#if NGRF > 0
		if (cdevsw[major(sb->st_rdev)].d_open == grfopen)
			ds.hst_rdev = grfdevno(sb->st_rdev);
#endif
#if NHIL > 0
		if (cdevsw[major(sb->st_rdev)].d_open == hilopen)
			ds.hst_rdev = hildevno(sb->st_rdev);
#endif
		;
	}
	if (sb->st_size < (quad_t)1 << 32)
		ds.hst_size = (long)sb->st_size;
	else
		ds.hst_size = -2;
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
 * HP-UX ioctl system call.  The differences here are:
 *	IOC_IN also means IOC_VOID if the size portion is zero.
 *	no FIOCLEX/FIONCLEX/FIOASYNC/FIOGETOWN/FIOSETOWN
 *	the sgttyb struct is 2 bytes longer
 */
struct hpuxioctl_args {
	int	fdes;
	int	cmd;
	caddr_t	cmarg;
};
hpuxioctl(p, uap, retval)
	struct proc *p;
	register struct hpuxioctl_args *uap;
	int *retval;
{
	register struct filedesc *fdp = p->p_fd;
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
		return (getsettty(p, uap->fdes, com, uap->cmarg));

	if (((unsigned)uap->fdes) >= fdp->fd_nfiles ||
	    (fp = fdp->fd_ofiles[uap->fdes]) == NULL)
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

	case HPUXFIOSNBIO:
	{
		char *ofp = &fdp->fd_ofileflags[uap->fdes];
		int tmp;

		if (*(int *)data)
			*ofp |= UF_FIONBIO_ON;
		else
			*ofp &= ~UF_FIONBIO_ON;
		/*
		 * Only set/clear if FNONBLOCK not in effect
		 */
		if ((*ofp & UF_FNDELAY_ON) == 0) {
			tmp = fp->f_flag & FNONBLOCK;
			error = (*fp->f_ops->fo_ioctl)(fp, FIONBIO,
						       (caddr_t)&tmp, p);
		}
		break;
	}

	case HPUXTIOCCONS:
		*(int *)data = 1;
		error = (*fp->f_ops->fo_ioctl)(fp, TIOCCONS, data, p);
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
		error = (*fp->f_ops->fo_ioctl)
			(fp, hpuxtobsdioctl(com), data, p);
		if (error == 0 && com == HPUXTIOCLGET) {
			*(int *)data &= LTOSTOP;
			if (*(int *)data & LTOSTOP)
				*(int *)data = HPUXLTOSTOP;
		}
		break;

	/* SYS 5 termio and POSIX termios */
	case HPUXTCGETA:
	case HPUXTCSETA:
	case HPUXTCSETAW:
	case HPUXTCSETAF:
	case HPUXTCGETATTR:
	case HPUXTCSETATTR:
	case HPUXTCSETATTRD:
	case HPUXTCSETATTRF:
		error = hpuxtermio(fp, com, data, p);
		break;

	default:
		error = (*fp->f_ops->fo_ioctl)(fp, com, data, p);
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
struct hpuxgetcontext_args {
	char *buf;
	int len;
};
hpuxgetcontext(p, uap, retval)
	struct proc *p;
	struct hpuxgetcontext_args *uap;
	int *retval;
{
	int error = 0;
	register int len;

#if defined(HP380)
	if (machineid == HP_380) {
		len = min(uap->len, sizeof(hpux040context));
		if (len)
			error = copyout(hpux040context, uap->buf, len);
		if (error == 0)
			*retval = sizeof(hpux040context);
		return (error);
	}
#endif
	len = min(uap->len, sizeof(hpuxcontext));
	if (len)
		error = copyout(hpuxcontext, uap->buf, (u_int)len);
	if (error == 0)
		*retval = sizeof(hpuxcontext);
	return (error);
}

/*
 * This is the equivalent of BSD getpgrp but with more restrictions.
 * Note we do not check the real uid or "saved" uid.
 */
struct hpuxgetpgrp2_args {
	int pid;
};
hpuxgetpgrp2(cp, uap, retval)
	struct proc *cp;
	register struct hpuxgetpgrp2_args *uap;
	int *retval;
{
	register struct proc *p;

	if (uap->pid == 0)
		uap->pid = cp->p_pid;
	p = pfind(uap->pid);
	if (p == 0)
		return (ESRCH);
	if (cp->p_ucred->cr_uid && p->p_ucred->cr_uid != cp->p_ucred->cr_uid &&
	    !inferior(p))
		return (EPERM);
	*retval = p->p_pgid;
	return (0);
}

/*
 * This is the equivalent of BSD setpgrp but with more restrictions.
 * Note we do not check the real uid or "saved" uid or pgrp.
 */
struct hpuxsetpgrp2_args {
	int	pid;
	int	pgrp;
};
hpuxsetpgrp2(p, uap, retval)
	struct proc *p;
	struct hpuxsetpgrp2_args *uap;
	int *retval;
{
	/* empirically determined */
	if (uap->pgrp < 0 || uap->pgrp >= 30000)
		return (EINVAL);
	return (setpgid(p, uap, retval));
}

/*
 * XXX Same as old BSD setre[ug]id right now.  Need to consider saved ids.
 */
struct hpuxsetresuid_args {
	int	ruid;
	int	euid;
	int	suid;
};
/* ARGSUSED */
hpuxsetresuid(p, uap, retval)
	register struct proc *p;
	struct hpuxsetresuid_args *uap;
	int *retval;
{
	register struct pcred *pc = p->p_cred;
	register uid_t ruid, euid;
	int error;

	if (uap->ruid == -1)
		ruid = pc->p_ruid;
	else
		ruid = uap->ruid;
	/*
	 * Allow setting real uid to previous effective, for swapping real and
	 * effective.  This should be:
	 *
	 * if (ruid != pc->p_ruid &&
	 *     (error = suser(pc->pc_ucred, &p->p_acflag)))
	 */
	if (ruid != pc->p_ruid && ruid != pc->pc_ucred->cr_uid /* XXX */ &&
	    (error = suser(pc->pc_ucred, &p->p_acflag)))
		return (error);
	if (uap->euid == -1)
		euid = pc->pc_ucred->cr_uid;
	else
		euid = uap->euid;
	if (euid != pc->pc_ucred->cr_uid && euid != pc->p_ruid &&
	    euid != pc->p_svuid && (error = suser(pc->pc_ucred, &p->p_acflag)))
		return (error);
	/*
	 * Everything's okay, do it.  Copy credentials so other references do
	 * not see our changes.
	 */
	pc->pc_ucred = crcopy(pc->pc_ucred);
	pc->pc_ucred->cr_uid = euid;
	pc->p_ruid = ruid;
	p->p_flag |= SUGID;
	return (0);
}

struct hpuxsetresgid_args {
	int	rgid;
	int	egid;
	int	sgid;
};
/* ARGSUSED */
hpuxsetresgid(p, uap, retval)
	register struct proc *p;
	struct hpuxsetresgid_args *uap;
	int *retval;
{
	register struct pcred *pc = p->p_cred;
	register gid_t rgid, egid;
	int error;

	if (uap->rgid == -1)
		rgid = pc->p_rgid;
	else
		rgid = uap->rgid;
	/*
	 * Allow setting real gid to previous effective, for swapping real and
	 * effective.  This didn't really work correctly in 4.[23], but is
	 * preserved so old stuff doesn't fail.  This should be:
	 *
	 * if (rgid != pc->p_rgid &&
	 *     (error = suser(pc->pc_ucred, &p->p_acflag)))
	 */
	if (rgid != pc->p_rgid && rgid != pc->pc_ucred->cr_groups[0] /* XXX */ &&
	    (error = suser(pc->pc_ucred, &p->p_acflag)))
		return (error);
	if (uap->egid == -1)
		egid = pc->pc_ucred->cr_groups[0];
	else
		egid = uap->egid;
	if (egid != pc->pc_ucred->cr_groups[0] && egid != pc->p_rgid &&
	    egid != pc->p_svgid && (error = suser(pc->pc_ucred, &p->p_acflag)))
		return (error);
	pc->pc_ucred = crcopy(pc->pc_ucred);
	pc->pc_ucred->cr_groups[0] = egid;
	pc->p_rgid = rgid;
	p->p_flag |= SUGID;
	return (0);
}

struct hpuxrlimit_args {
	u_int	which;
	struct	orlimit *rlp;
};
hpuxgetrlimit(p, uap, retval)
	struct proc *p;
	struct hpuxrlimit_args *uap;
	int *retval;
{
	if (uap->which > HPUXRLIMIT_NOFILE)
		return (EINVAL);
	if (uap->which == HPUXRLIMIT_NOFILE)
		uap->which = RLIMIT_NOFILE;
	return (getrlimit(p, uap, retval));
}

hpuxsetrlimit(p, uap, retval)
	struct proc *p;
	struct hpuxrlimit_args *uap;
	int *retval;
{
	if (uap->which > HPUXRLIMIT_NOFILE)
		return (EINVAL);
	if (uap->which == HPUXRLIMIT_NOFILE)
		uap->which = RLIMIT_NOFILE;
	return (setrlimit(p, uap, retval));
}

/*
 * XXX: simple recognition hack to see if we can make grmd work.
 */
struct hpuxlockf_args {
	int fd;
	int func;
	long size;
};
hpuxlockf(p, uap, retval)
	struct proc *p;
	struct hpuxlockf_args *uap;
	int *retval;
{
	return (0);
}

struct hpuxgetaccess_args {
	char	*path;
	int	uid;
	int	ngroups;
	int	*gidset;
	void	*label;
	void	*privs;
};
hpuxgetaccess(p, uap, retval)
	register struct proc *p;
	register struct hpuxgetaccess_args *uap;
	int *retval;
{
	int lgroups[NGROUPS];
	int error = 0;
	register struct ucred *cred;
	register struct vnode *vp;
	struct nameidata nd;

	/*
	 * Build an appropriate credential structure
	 */
	cred = crdup(p->p_ucred);
	switch (uap->uid) {
	case 65502:	/* UID_EUID */
		break;
	case 65503:	/* UID_RUID */
		cred->cr_uid = p->p_cred->p_ruid;
		break;
	case 65504:	/* UID_SUID */
		error = EINVAL;
		break;
	default:
		if (uap->uid > 65504)
			error = EINVAL;
		cred->cr_uid = uap->uid;
		break;
	}
	switch (uap->ngroups) {
	case -1:	/* NGROUPS_EGID */
		cred->cr_ngroups = 1;
		break;
	case -5:	/* NGROUPS_EGID_SUPP */
		break;
	case -2:	/* NGROUPS_RGID */
		cred->cr_ngroups = 1;
		cred->cr_gid = p->p_cred->p_rgid;
		break;
	case -6:	/* NGROUPS_RGID_SUPP */
		cred->cr_gid = p->p_cred->p_rgid;
		break;
	case -3:	/* NGROUPS_SGID */
	case -7:	/* NGROUPS_SGID_SUPP */
		error = EINVAL;
		break;
	case -4:	/* NGROUPS_SUPP */
		if (cred->cr_ngroups > 1)
			cred->cr_gid = cred->cr_groups[1];
		else
			error = EINVAL;
		break;
	default:
		if (uap->ngroups > 0 && uap->ngroups <= NGROUPS)
			error = copyin((caddr_t)uap->gidset,
				       (caddr_t)&lgroups[0],
				       uap->ngroups * sizeof(lgroups[0]));
		else
			error = EINVAL;
		if (error == 0) {
			int gid;

			for (gid = 0; gid < uap->ngroups; gid++)
				cred->cr_groups[gid] = lgroups[gid];
			cred->cr_ngroups = uap->ngroups;
		}
		break;
	}
	/*
	 * Lookup file using caller's effective IDs.
	 */
	if (error == 0) {
		NDINIT(&nd, LOOKUP, FOLLOW | LOCKLEAF, UIO_USERSPACE,
			uap->path, p);
		error = namei(&nd);
	}
	if (error) {
		crfree(cred);
		return (error);
	}
	/*
	 * Use the constructed credentials for access checks.
	 */
	vp = nd.ni_vp;
	*retval = 0;
	if (VOP_ACCESS(vp, VREAD, cred, p) == 0)
		*retval |= R_OK;
	if (vn_writechk(vp) == 0 && VOP_ACCESS(vp, VWRITE, cred, p) == 0)
		*retval |= W_OK;
	/* XXX we return X_OK for root on VREG even if not */
	if (VOP_ACCESS(vp, VEXEC, cred, p) == 0)
		*retval |= X_OK;
	vput(vp);
	crfree(cred);
	return (error);
}

extern char kstack[];
#define UOFF(f)		((int)&((struct user *)0)->f)
#define HPUOFF(f)	((int)&((struct hpuxuser *)0)->f)

/* simplified FP structure */
struct bsdfp {
	int save[54];
	int reg[24];
	int ctrl[3];
};

/*
 * Brutal hack!  Map HP-UX u-area offsets into BSD k-stack offsets.
 */
hpuxtobsduoff(off, isps, p)
	int *off, *isps;
	struct proc *p;
{
	register int *ar0 = p->p_md.md_regs;
	struct hpuxfp *hp;
	struct bsdfp *bp;
	register u_int raddr;

	*isps = 0;

	/* u_ar0 field; procxmt puts in U_ar0 */
	if ((int)off == HPUOFF(hpuxu_ar0))
		return(UOFF(U_ar0));

#ifdef FPCOPROC
	/* FP registers from PCB */
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
		off = (int *)((u_int)off + (u_int)kstack);

	/*
	 * General registers.
	 * We know that the HP-UX registers are in the same order as ours.
	 * The only difference is that their PS is 2 bytes instead of a
	 * padded 4 like ours throwing the alignment off.
	 */
	if (off >= ar0 && off < &ar0[18]) {
		/*
		 * PS: return low word and high word of PC as HP-UX would
		 * (e.g. &u.u_ar0[16.5]).
		 *
		 * XXX we don't do this since HP-UX adb doesn't rely on
		 * it and passing such an offset to procxmt will cause
		 * it to fail anyway.  Instead, we just set the offset
		 * to PS and let hpuxptrace() shift up the value returned.
		 */
		if (off == &ar0[PS]) {
#if 0
			raddr = (u_int) &((short *)ar0)[PS*2+1];
#else
			raddr = (u_int) &ar0[(int)(off - ar0)];
#endif
			*isps = 1;
		}
		/*
		 * PC: off will be &u.u_ar0[16.5] since HP-UX saved PS
		 * is only 16 bits.
		 */
		else if (off == (int *)&(((short *)ar0)[PS*2+1]))
			raddr = (u_int) &ar0[PC];
		/*
		 * D0-D7, A0-A7: easy
		 */
		else
			raddr = (u_int) &ar0[(int)(off - ar0)];
		return((int)(raddr - (u_int)kstack));
	}

	/* everything else */
	return(-1);
}

/*
 * Kludge up a uarea dump so that HP-UX debuggers can find out
 * what they need.  IMPORTANT NOTE: we do not EVEN attempt to
 * convert the entire user struct.
 */
hpuxdumpu(vp, cred)
	struct vnode *vp;
	struct ucred *cred;
{
	struct proc *p = curproc;
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
	faku->hpuxu_tsize = p->p_vmspace->vm_tsize;
	faku->hpuxu_dsize = p->p_vmspace->vm_dsize;
	faku->hpuxu_ssize = p->p_vmspace->vm_ssize;
	/*
	 * Fill in the exec header for CDB.
	 * This was saved back in exec().  As far as I can tell CDB
	 * only uses this information to verify that a particular
	 * core file goes with a particular binary.
	 */
	bcopy((caddr_t)p->p_addr->u_md.md_exec,
	      (caddr_t)&faku->hpuxu_exdata, sizeof (struct hpux_exec));
	/*
	 * Adjust user's saved registers (on kernel stack) to reflect
	 * HP-UX order.  Note that HP-UX saves the SR as 2 bytes not 4
	 * so we have to move it up.
	 */
	faku->hpuxu_ar0 = p->p_md.md_regs;
	foop = (short *) p->p_md.md_regs;
	foop[32] = foop[33];
	foop[33] = foop[34];
	foop[34] = foop[35];
#ifdef FPCOPROC
	/*
	 * Copy 68881 registers from our PCB format to HP-UX format
	 */
	bp = (struct bsdfp *) &p->p_addr->u_pcb.pcb_fpregs;
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
	error = vn_rdwr(UIO_WRITE, vp, (caddr_t)faku, ctob(1), (off_t)0,
			UIO_SYSSPACE, IO_NODELOCKED|IO_UNIT, cred,
			(int *)NULL, p);
	/*
	 * Dump the remaining UPAGES-1 pages normally
	 */
	if (!error) 
		error = vn_rdwr(UIO_WRITE, vp, kstack + ctob(1),
				ctob(UPAGES-1), (off_t)ctob(1), UIO_SYSSPACE,
				IO_NODELOCKED|IO_UNIT, cred, (int *)NULL, p);
	free((caddr_t)faku, M_TEMP);
	return(error);
}

/*
 * The remaining routines are essentially the same as those in kern_xxx.c
 * and vfs_xxx.c as defined under "#ifdef COMPAT".  We replicate them here
 * to avoid HPUXCOMPAT dependencies in those files and to make sure that
 * HP-UX compatibility still works even when COMPAT is not defined.
 *
 * These are still needed as of HP-UX 7.05.
 */
#ifdef COMPAT_OHPUX

#define HPUX_HZ	50

#include "sys/times.h"

/* from old timeb.h */
struct hpuxtimeb {
	time_t	time;
	u_short	millitm;
	short	timezone;
	short	dstflag;
};

/* ye ole stat structure */
struct	ohpuxstat {
	u_short	ohst_dev;
	u_short	ohst_ino;
	u_short ohst_mode;
	short  	ohst_nlink;
	short  	ohst_uid;
	short  	ohst_gid;
	u_short	ohst_rdev;
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
		enterpgrp(p, p->p_pid, 0);
	*retval = p->p_pgid;
	return (0);
}

struct ohpuxtime_args {
	long	*tp;
};
ohpuxtime(p, uap, retval)
	struct proc *p;
	register struct ohpuxtime_args *uap;
	int *retval;
{
	int error = 0;

	if (uap->tp)
		error = copyout((caddr_t)&time.tv_sec, (caddr_t)uap->tp,
				sizeof (long));
	*(time_t *)retval = time.tv_sec;
	return (error);
}

struct ohpuxstime_args {
	int	time;
};
ohpuxstime(p, uap, retval)
	struct proc *p;
	register struct ohpuxstime_args *uap;
	int *retval;
{
	struct timeval tv;
	int s, error;

	tv.tv_sec = uap->time;
	tv.tv_usec = 0;
	if (error = suser(p->p_ucred, &p->p_acflag))
		return (error);

	/* WHAT DO WE DO ABOUT PENDING REAL-TIME TIMEOUTS??? */
	boottime.tv_sec += tv.tv_sec - time.tv_sec;
	s = splhigh(); time = tv; splx(s);
	resettodr();
	return (0);
}

struct ohpuxftime_args {
	struct	hpuxtimeb *tp;
};
ohpuxftime(p, uap, retval)
	struct proc *p;
	register struct ohpuxftime_args *uap;
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

struct ohpuxalarm_args {
	int	deltat;
};
ohpuxalarm(p, uap, retval)
	register struct proc *p;
	register struct ohpuxalarm_args *uap;
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

struct ohpuxnice_args {
	int	niceness;
};
ohpuxnice(p, uap, retval)
	register struct proc *p;
	register struct ohpuxnice_args *uap;
	int *retval;
{
	int error;

	error = donice(p, p, (p->p_nice-NZERO)+uap->niceness);
	if (error == 0)
		*retval = p->p_nice - NZERO;
	return (error);
}

struct ohpuxtimes_args {
	struct	tms *tmsb;
};
ohpuxtimes(p, uap, retval)
	struct proc *p;
	register struct ohpuxtimes_args *uap;
	int *retval;
{
	struct timeval ru, rs;
	struct tms atms;
	int error;

	calcru(p, &ru, &rs, NULL);
	atms.tms_utime = hpuxscale(&ru);
	atms.tms_stime = hpuxscale(&rs);
	atms.tms_cutime = hpuxscale(&p->p_stats->p_cru.ru_utime);
	atms.tms_cstime = hpuxscale(&p->p_stats->p_cru.ru_stime);
	error = copyout((caddr_t)&atms, (caddr_t)uap->tmsb, sizeof (atms));
	if (error == 0)
		*(time_t *)retval = hpuxscale(&time) - hpuxscale(&boottime);
	return (error);
}

/*
 * Doesn't exactly do what the documentation says.
 * What we really do is return 1/HPUX_HZ-th of a second since that
 * is what HP-UX returns.
 */
hpuxscale(tvp)
	register struct timeval *tvp;
{
	return (tvp->tv_sec * HPUX_HZ + tvp->tv_usec * HPUX_HZ / 1000000);
}

/*
 * Set IUPD and IACC times on file.
 * Can't set ICHG.
 */
struct ohpuxutime_args {
	char	*fname;
	time_t	*tptr;
};
ohpuxutime(p, uap, retval)
	struct proc *p;
	register struct ohpuxutime_args *uap;
	int *retval;
{
	register struct vnode *vp;
	struct vattr vattr;
	time_t tv[2];
	int error;
	struct nameidata nd;

	if (uap->tptr) {
		error = copyin((caddr_t)uap->tptr, (caddr_t)tv, sizeof (tv));
		if (error)
			return (error);
	} else
		tv[0] = tv[1] = time.tv_sec;
	vattr_null(&vattr);
	vattr.va_atime.ts_sec = tv[0];
	vattr.va_atime.ts_nsec = 0;
	vattr.va_mtime.ts_sec = tv[1];
	vattr.va_mtime.ts_nsec = 0;
	NDINIT(&nd, LOOKUP, FOLLOW | LOCKLEAF, UIO_USERSPACE, uap->fname, p);
	if (error = namei(&nd))
		return (error);
	vp = nd.ni_vp;
	if (vp->v_mount->mnt_flag & MNT_RDONLY)
		error = EROFS;
	else
		error = VOP_SETATTR(vp, &vattr, nd.ni_cnd.cn_cred, p);
	vput(vp);
	return (error);
}

ohpuxpause(p, uap, retval)
	struct proc *p;
	int *uap, *retval;
{
	(void) tsleep(kstack, PPAUSE | PCATCH, "pause", 0);
	/* always return EINTR rather than ERESTART... */
	return (EINTR);
}

/*
 * The old fstat system call.
 */
struct ohpuxfstat_args {
	int	fd;
	struct ohpuxstat *sb;
};
ohpuxfstat(p, uap, retval)
	struct proc *p;
	register struct ohpuxfstat_args *uap;
	int *retval;
{
	register struct filedesc *fdp = p->p_fd;
	struct file *fp;

	if (((unsigned)uap->fd) >= fdp->fd_nfiles ||
	    (fp = fdp->fd_ofiles[uap->fd]) == NULL)
		return (EBADF);
	if (fp->f_type != DTYPE_VNODE)
		return (EINVAL);
	return (ohpuxstat1((struct vnode *)fp->f_data, uap->sb, p));
}

/*
 * Old stat system call.  This version follows links.
 */
struct ohpuxstat_args {
	char	*fname;
	struct ohpuxstat *sb;
};
ohpuxstat(p, uap, retval)
	struct proc *p;
	register struct ohpuxstat_args *uap;
	int *retval;
{
	int error;
	struct nameidata nd;

	NDINIT(&nd, LOOKUP, FOLLOW | LOCKLEAF, UIO_USERSPACE, uap->fname, p);
	if (error = namei(&nd))
		return (error);
	error = ohpuxstat1(nd.ni_vp, uap->sb, p);
	vput(nd.ni_vp);
	return (error);
}

int
ohpuxstat1(vp, ub, p)
	struct vnode *vp;
	struct ohpuxstat *ub;
	struct proc *p;
{
	struct ohpuxstat ohsb;
	struct stat sb;
	int error;

	error = vn_stat(vp, &sb, p);
	if (error)
		return (error);

	ohsb.ohst_dev = sb.st_dev;
	ohsb.ohst_ino = sb.st_ino;
	ohsb.ohst_mode = sb.st_mode;
	ohsb.ohst_nlink = sb.st_nlink;
	ohsb.ohst_uid = sb.st_uid;
	ohsb.ohst_gid = sb.st_gid;
	ohsb.ohst_rdev = sb.st_rdev;
	if (sb.st_size < (quad_t)1 << 32)
		ohsb.ohst_size = sb.st_size;
	else
		ohsb.ohst_size = -2;
	ohsb.ohst_atime = sb.st_atime;
	ohsb.ohst_mtime = sb.st_mtime;
	ohsb.ohst_ctime = sb.st_ctime;
	return (copyout((caddr_t)&ohsb, (caddr_t)ub, sizeof(ohsb)));
}
#endif
#endif

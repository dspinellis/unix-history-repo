/*
 * Copyright (c) 1982, 1986, 1989 Regents of the University of California.
 * All rights reserved.
 *
 * %sccs.include.redist.c%
 *
 *	@(#)kern_sysctl.c	7.25 (Berkeley) %G%
 */

#include <sys/param.h>
#include <sys/proc.h>
#include <sys/kinfo.h>
#include <sys/ioctl.h>
#include <sys/tty.h>
#include <sys/buf.h>
#include <sys/file.h>

#include <vm/vm.h>

#include <sys/kinfo_proc.h>

#define snderr(e) { error = (e); goto release;}
extern int kinfo_doproc(), kinfo_rtable(), kinfo_vnode(), kinfo_file();
extern int kinfo_meter(), kinfo_loadavg(), kinfo_clockrate();
struct kinfo_lock kinfo_lock;

struct getkerninfo_args {
	int	op;
	char	*where;
	int	*size;
	int	arg;
};
/* ARGSUSED */
getkerninfo(p, uap, retval)
	struct proc *p;
	register struct getkerninfo_args *uap;
	int *retval;
{
	int bufsize;		/* max size of users buffer */
	int needed, locked, (*server)(), error = 0;

	switch (ki_type(uap->op)) {

	case KINFO_PROC:
		server = kinfo_doproc;
		break;

	case KINFO_RT:
		server = kinfo_rtable;
		break;

	case KINFO_VNODE:
		server = kinfo_vnode;
		break;

	case KINFO_FILE:
		server = kinfo_file;
		break;

	case KINFO_METER:
		server = kinfo_meter;
		break;

	case KINFO_LOADAVG:
		server = kinfo_loadavg;
		break;

	case KINFO_CLOCKRATE:
		server = kinfo_clockrate;
		break;

	default:
		error = EINVAL;
		goto done;
	}
	if (uap->where == NULL || uap->size == NULL) {
		error = (*server)(uap->op, NULL, NULL, uap->arg, &needed);
		goto done;
	}
	if (error = copyin((caddr_t)uap->size, (caddr_t)&bufsize,
	    sizeof (bufsize)))
		goto done;
	while (kinfo_lock.kl_lock) {
		kinfo_lock.kl_want = 1;
		sleep((caddr_t)&kinfo_lock, PRIBIO+1);
		kinfo_lock.kl_locked++;
	}
	kinfo_lock.kl_lock = 1;

	if (!useracc(uap->where, bufsize, B_WRITE))
		snderr(EFAULT);
	if (server != kinfo_vnode)	/* XXX */
		vslock(uap->where, bufsize);
	locked = bufsize;
	error = (*server)(uap->op, uap->where, &bufsize, uap->arg, &needed);
	if (server != kinfo_vnode)	/* XXX */
		vsunlock(uap->where, locked, B_WRITE);
	if (error == 0)
		error = copyout((caddr_t)&bufsize,
				(caddr_t)uap->size, sizeof (bufsize));
release:
	kinfo_lock.kl_lock = 0;
	if (kinfo_lock.kl_want) {
		kinfo_lock.kl_want = 0;
		wakeup((caddr_t)&kinfo_lock);
	}
done:
	if (!error)
		*retval = needed;
	return (error);
}

/* 
 * try over estimating by 5 procs 
 */
#define KINFO_PROCSLOP	(5 * sizeof (struct kinfo_proc))

kinfo_doproc(op, where, acopysize, arg, aneeded)
	int op;
	char *where;
	int *acopysize, arg, *aneeded;
{
	register struct proc *p;
	register struct kinfo_proc *dp = (struct kinfo_proc *)where;
	register needed = 0;
	int buflen = where != NULL ? *acopysize : 0;
	int doingzomb;
	struct eproc eproc;
	int error = 0;

	p = (struct proc *)allproc;
	doingzomb = 0;
again:
	for (; p != NULL; p = p->p_nxt) {
		/*
		 * Skip embryonic processes.
		 */
		if (p->p_stat == SIDL)
			continue;
		/* 
		 * TODO - make more efficient (see notes below).
		 * do by session. 
		 */
		switch (ki_op(op)) {

		case KINFO_PROC_PID:
			/* could do this with just a lookup */
			if (p->p_pid != (pid_t)arg)
				continue;
			break;

		case KINFO_PROC_PGRP:
			/* could do this by traversing pgrp */
			if (p->p_pgrp->pg_id != (pid_t)arg)
				continue;
			break;

		case KINFO_PROC_TTY:
			if ((p->p_flag&SCTTY) == 0 || 
			    p->p_session->s_ttyp == NULL ||
			    p->p_session->s_ttyp->t_dev != (dev_t)arg)
				continue;
			break;

		case KINFO_PROC_UID:
			if (p->p_ucred->cr_uid != (uid_t)arg)
				continue;
			break;

		case KINFO_PROC_RUID:
			if (p->p_cred->p_ruid != (uid_t)arg)
				continue;
			break;
		}
		if (buflen >= sizeof (struct kinfo_proc)) {
			fill_eproc(p, &eproc);
			if (error = copyout((caddr_t)p, &dp->kp_proc, 
			    sizeof (struct proc)))
				return (error);
			if (error = copyout((caddr_t)&eproc, &dp->kp_eproc, 
			    sizeof (eproc)))
				return (error);
			dp++;
			buflen -= sizeof (struct kinfo_proc);
		}
		needed += sizeof (struct kinfo_proc);
	}
	if (doingzomb == 0) {
		p = zombproc;
		doingzomb++;
		goto again;
	}
	if (where != NULL)
		*acopysize = (caddr_t)dp - where;
	else
		needed += KINFO_PROCSLOP;
	*aneeded = needed;

	return (0);
}

/*
 * Fill in an eproc structure for the specified process.
 */
void
fill_eproc(p, ep)
	register struct proc *p;
	register struct eproc *ep;
{
	register struct tty *tp;

	ep->e_paddr = p;
	ep->e_sess = p->p_pgrp->pg_session;
	ep->e_pcred = *p->p_cred;
	ep->e_ucred = *p->p_ucred;
	if (p->p_stat == SIDL || p->p_stat == SZOMB) {
		ep->e_vm.vm_rssize = 0;
		ep->e_vm.vm_tsize = 0;
		ep->e_vm.vm_dsize = 0;
		ep->e_vm.vm_ssize = 0;
#ifndef sparc
		/* ep->e_vm.vm_pmap = XXX; */
#endif
	} else {
		register struct vmspace *vm = p->p_vmspace;

		ep->e_vm.vm_rssize = vm->vm_rssize;
		ep->e_vm.vm_tsize = vm->vm_tsize;
		ep->e_vm.vm_dsize = vm->vm_dsize;
		ep->e_vm.vm_ssize = vm->vm_ssize;
#ifndef sparc
		ep->e_vm.vm_pmap = vm->vm_pmap;
#endif
	}
	if (p->p_pptr)
		ep->e_ppid = p->p_pptr->p_pid;
	else
		ep->e_ppid = 0;
	ep->e_pgid = p->p_pgrp->pg_id;
	ep->e_jobc = p->p_pgrp->pg_jobc;
	if ((p->p_flag&SCTTY) && 
	     (tp = ep->e_sess->s_ttyp)) {
		ep->e_tdev = tp->t_dev;
		ep->e_tpgid = tp->t_pgrp ? tp->t_pgrp->pg_id : NO_PID;
		ep->e_tsess = tp->t_session;
	} else
		ep->e_tdev = NODEV;
	ep->e_flag = ep->e_sess->s_ttyvp ? EPROC_CTTY : 0;
	if (SESS_LEADER(p))
		ep->e_flag |= EPROC_SLEADER;
	if (p->p_wmesg)
		strncpy(ep->e_wmesg, p->p_wmesg, WMESGLEN);
	ep->e_xsize = ep->e_xrssize = 0;
	ep->e_xccount = ep->e_xswrss = 0;
}

/*
 * Get file structures.
 */
kinfo_file(op, where, acopysize, arg, aneeded)
	int op;
	register char *where;
	int *acopysize, arg, *aneeded;
{
	int buflen, needed, error;
	struct file *fp;
	char *start = where;

	if (where == NULL) {
		/*
		 * overestimate by 10 files
		 */
		*aneeded = sizeof (filehead) + 
			(nfiles + 10) * sizeof (struct file);
		return (0);
	}
	buflen = *acopysize;
	needed = 0;

	/*
	 * first copyout filehead
	 */
	if (buflen > sizeof (filehead)) {
		if (error = copyout((caddr_t)&filehead, where,
		    sizeof (filehead)))
			return (error);
		buflen -= sizeof (filehead);
		where += sizeof (filehead);
	}
	needed += sizeof (filehead);

	/*
	 * followed by an array of file structures
	 */
	for (fp = filehead; fp != NULL; fp = fp->f_filef) {
		if (buflen > sizeof (struct file)) {
			if (error = copyout((caddr_t)fp, where,
			    sizeof (struct file)))
				return (error);
			buflen -= sizeof (struct file);
			where += sizeof (struct file);
		}
		needed += sizeof (struct file);
	}
	*acopysize = where - start;
	*aneeded = needed;

	return (0);
}

/*-
 * Copyright (c) 1982, 1986, 1989, 1993 Regents of the University of California.
 * All rights reserved.
 *
 * This code is derived from software contributed to Berkeley by
 * Mike Karels at Berkeley Software Design, Inc.
 *
 * %sccs.include.redist.c%
 *
 *	@(#)kern_sysctl.c	7.26 (Berkeley) %G%
 */

/*
 * sysctl system call.
 */

#include <sys/param.h>
#include <sys/systm.h>
#include <sys/malloc.h>
#include <sys/proc.h>
#include <sys/file.h>
#include <sys/sysctl.h>
#include <sys/unistd.h>
#include <sys/buf.h>
#include <sys/ioctl.h>
#include <sys/tty.h>

#include <vm/vm.h>

#include <sys/kinfo_proc.h>

sysctlfn kern_sysctl;
sysctlfn hw_sysctl;
extern sysctlfn vm_sysctl;
extern sysctlfn fs_sysctl;
extern sysctlfn net_sysctl;
extern sysctlfn cpu_sysctl;

/*
 * Locking and stats
 */
static struct sysctl_lock {
	int	sl_lock;
	int	sl_want;
	int	sl_locked;
} memlock;

struct sysctl_args {
	int	*name;
	u_int	namelen;
	void	*old;
	u_int	*oldlenp;
	void	*new;
	u_int	newlen;
};

#define	STK_PARAMS	32	/* largest old/new values on stack */

sysctl(p, uap, retval)
	struct proc *p;
	register struct sysctl_args *uap;
	int *retval;
{
	int error, dolock = 1;
	u_int savelen, oldlen = 0;
	sysctlfn *fn;
	int name[CTL_MAXNAME];

	if (uap->new != NULL && (error = suser(p->p_ucred, &p->p_acflag)))
		return (error);
	/*
	 * all top-level sysctl names are non-terminal
	 */
	if (uap->namelen > CTL_MAXNAME || uap->namelen < 2)
		return (EINVAL);
	if (error = copyin(uap->name, &name, uap->namelen * sizeof(int)))
		return (error);

	switch (name[0]) {
	case CTL_KERN:
		fn = kern_sysctl;
		if (name[2] != KERN_VNODE)	/* XXX */
			dolock = 0;
		break;
	case CTL_HW:
		fn = hw_sysctl;
		break;
	case CTL_VM:
		fn = vm_sysctl;
		break;
	case CTL_NET:
		fn = net_sysctl;
		break;
#ifdef notyet
	case CTL_FS:
		fn = fs_sysctl;
		break;
	case CTL_DEBUG:
		fn = debug_sysctl;
		break;
	case CTL_MACHDEP:
		fn = cpu_sysctl;
		break;
#endif
	default:
		return (EOPNOTSUPP);
	}

	if (uap->oldlenp &&
	    (error = copyin(uap->oldlenp, &oldlen, sizeof(oldlen))))
		return (error);
	if (uap->old != NULL) {
		if (!useracc(uap->old, oldlen, B_WRITE))
			return (EFAULT);
		while (memlock.sl_lock) {
			memlock.sl_want = 1;
			sleep((caddr_t)&memlock, PRIBIO+1);
			memlock.sl_locked++;
		}
		memlock.sl_lock = 1;
		if (dolock)
			vslock(uap->old, oldlen);
		savelen = oldlen;
	}
	error = (*fn)(name + 1, uap->namelen - 1, uap->old, &oldlen,
	    uap->new, uap->newlen);
	if (uap->old != NULL) {
		if (dolock)
			vsunlock(uap->old, savelen, B_WRITE);
		memlock.sl_lock = 0;
		if (memlock.sl_want) {
			memlock.sl_want = 0;
			wakeup((caddr_t)&memlock);
		}
	}
	if (error)
		return (error);
	if (uap->oldlenp)
		error = copyout(&oldlen, uap->oldlenp, sizeof(oldlen));
	*retval = oldlen;
	return (0);
}

/*
 * Attributes stored in the kernel.
 */
char hostname[MAXHOSTNAMELEN];
int hostnamelen;
long hostid;

kern_sysctl(name, namelen, oldp, oldlenp, newp, newlen)
	int *name;
	u_int namelen;
	void *oldp;
	u_int *oldlenp;
	void *newp;
	u_int newlen;
{
	int error;
	extern char ostype[], osrelease[], version[];

	/* all sysctl names at this level are terminal */
	if (namelen != 1 && name[0] != KERN_PROC)
		return (ENOTDIR);		/* overloaded */

	switch (name[0]) {
	case KERN_OSTYPE:
		return (sysctl_rdstring(oldp, oldlenp, newp, ostype));
	case KERN_OSRELEASE:
		return (sysctl_rdstring(oldp, oldlenp, newp, osrelease));
	case KERN_OSREV:
		return (sysctl_rdint(oldp, oldlenp, newp, BSD));
	case KERN_VERSION:
		return (sysctl_rdstring(oldp, oldlenp, newp, version));
	case KERN_POSIX1:
		return (sysctl_rdint(oldp, oldlenp, newp, _POSIX_VERSION));
	case KERN_MAXPROC:
		return (sysctl_int(oldp, oldlenp, newp, newlen, &maxproc));
	case KERN_MAXFILES:
		return (sysctl_int(oldp, oldlenp, newp, newlen, &maxfiles));
	case KERN_ARGMAX:
		return (sysctl_rdint(oldp, oldlenp, newp, ARG_MAX));
	case KERN_HOSTNAME:
		error = sysctl_string(oldp, oldlenp, newp, newlen, 
		    hostname, sizeof(hostname));
		if (!error)
			hostnamelen = newlen;
		return (error);
	case KERN_HOSTID:
		return (sysctl_int(oldp, oldlenp, newp, newlen, &hostid));
	case KERN_CLOCKRATE:
		return (sysctl_clockrate(oldp, oldlenp));
	case KERN_FILE:
		return (sysctl_file(oldp, oldlenp));
	case KERN_VNODE:
		return (sysctl_vnode(oldp, oldlenp));
	case KERN_PROC:
		return (sysctl_doproc(name + 1, namelen - 1, oldp, oldlenp));
	default:
		return (EOPNOTSUPP);
	}
	/* NOTREACHED */
}

hw_sysctl(name, namelen, oldp, oldlenp, newp, newlen)
	int *name;
	u_int namelen;
	void *oldp;
	u_int *oldlenp;
	void *newp;
	u_int newlen;
{
	extern char machine[], cpu_model[];

	/* all sysctl names at this level are terminal */
	if (namelen != 1)
		return (ENOTDIR);		/* overloaded */

	switch (name[0]) {
	case HW_MACHINE:
		return (sysctl_rdstring(oldp, oldlenp, newp, machine));
	case HW_MODEL:
		return (sysctl_rdstring(oldp, oldlenp, newp, cpu_model));
	case HW_NCPU:
		return (sysctl_rdint(oldp, oldlenp, newp, 1));	/* XXX */
	case HW_CPUSPEED:
		return (sysctl_rdint(oldp, oldlenp, newp, cpuspeed));
	case HW_PHYSMEM:
		return (sysctl_rdint(oldp, oldlenp, newp, ctob(physmem)));
	case HW_USERMEM:
		return (sysctl_rdint(oldp, oldlenp, newp,
		    ctob(physmem - cnt.v_wire_count)));
	case HW_PAGESIZE:
		return (sysctl_rdint(oldp, oldlenp, newp, PAGE_SIZE));
	default:
		return (EOPNOTSUPP);
	}
	/* NOTREACHED */
}

/*
 * Validate parameters and get old / set new parameters
 * for an integer-valued sysctl function.
 */
sysctl_int(oldp, oldlenp, newp, newlen, valp)
	void *oldp;
	u_int *oldlenp;
	void *newp;
	u_int newlen;
	int *valp;
{
	int error = 0;

	if (oldp && *oldlenp < sizeof(int))
		return (ENOMEM);
	if (newp && newlen != sizeof(int))
		return (EINVAL);
	*oldlenp = sizeof(int);
	if (oldp)
		error = copyout(valp, oldp, sizeof(int));
	if (error == 0 && newp)
		error = copyin(newp, valp, sizeof(int));
	return (error);
}

/*
 * As above, but read-only.
 */
sysctl_rdint(oldp, oldlenp, newp, val)
	void *oldp;
	u_int *oldlenp;
	void *newp;
	int val;
{
	int error = 0;

	if (oldp && *oldlenp < sizeof(int))
		return (ENOMEM);
	if (newp)
		return (EPERM);
	*oldlenp = sizeof(int);
	if (oldp)
		error = copyout((caddr_t)&val, oldp, sizeof(int));
	return (error);
}

/*
 * Validate parameters and get old / set new parameters
 * for a string-valued sysctl function.
 */
sysctl_string(oldp, oldlenp, newp, newlen, str, maxlen)
	void *oldp;
	u_int *oldlenp;
	void *newp;
	u_int newlen;
	char *str;
	int maxlen;
{
	int len, error = 0;

	len = strlen(str) + 1;
	if (oldp && *oldlenp < len)
		return (ENOMEM);
	if (newp && newlen >= maxlen)
		return (EINVAL);
	*oldlenp = len;
	if (oldp)
		error = copyout(str, oldp, len);
	if (error == 0 && newp) {
		error = copyin(newp, str, newlen);
		str[newlen] = 0;
	}
	return (error);
}

/*
 * As above, but read-only.
 */
sysctl_rdstring(oldp, oldlenp, newp, str)
	void *oldp;
	u_int *oldlenp;
	void *newp;
	char *str;
{
	int len, error = 0;

	len = strlen(str) + 1;
	if (oldp && *oldlenp < len)
		return (ENOMEM);
	if (newp)
		return (EPERM);
	*oldlenp = len;
	if (oldp)
		error = copyout(str, oldp, len);
	return (error);
}

/*
 * Validate parameters and get old parameters
 * for a structure oriented sysctl function.
 */
sysctl_rdstruct(oldp, oldlenp, newp, sp, len)
	void *oldp;
	u_int *oldlenp;
	void *newp;
	void *sp;
	int len;
{
	int error = 0;

	if (oldp && *oldlenp < len)
		return (ENOMEM);
	if (newp)
		return (EPERM);
	*oldlenp = len;
	if (oldp)
		error = copyout(sp, oldp, len);
	return (error);
}

/*
 * Get file structures.
 */
sysctl_file(where, sizep)
	char *where;
	int *sizep;
{
	int buflen, error;
	struct file *fp;
	char *start = where;

	buflen = *sizep;
	if (where == NULL) {
		/*
		 * overestimate by 10 files
		 */
		*sizep = sizeof(filehead) + (nfiles + 10) * sizeof(struct file);
		return (0);
	}

	/*
	 * first copyout filehead
	 */
	if (buflen < sizeof(filehead)) {
		*sizep = 0;
		return (0);
	}
	if (error = copyout((caddr_t)&filehead, where, sizeof(filehead)))
		return (error);
	buflen += sizeof(filehead);
	where += sizeof(filehead);

	/*
	 * followed by an array of file structures
	 */
	for (fp = filehead; fp != NULL; fp = fp->f_filef) {
		if (buflen < sizeof(struct file)) {
			*sizep = where - start;
			return (ENOMEM);
		}
		if (error = copyout((caddr_t)fp, where, sizeof (struct file)))
			return (error);
		buflen -= sizeof(struct file);
		where += sizeof(struct file);
	}
	*sizep = where - start;
	return (0);
}

/* 
 * try over estimating by 5 procs 
 */
#define KERN_PROCSLOP	(5 * sizeof (struct kinfo_proc))

sysctl_doproc(name, namelen, where, sizep)
	int *name;
	int namelen;
	char *where;
	int *sizep;
{
	register struct proc *p;
	register struct kinfo_proc *dp = (struct kinfo_proc *)where;
	register int needed = 0;
	int buflen = where != NULL ? *sizep : 0;
	int doingzomb;
	struct eproc eproc;
	int error = 0;

	if (namelen != 2)
		return (EINVAL);
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
		switch (name[0]) {

		case KERN_PROC_PID:
			/* could do this with just a lookup */
			if (p->p_pid != (pid_t)name[1])
				continue;
			break;

		case KERN_PROC_PGRP:
			/* could do this by traversing pgrp */
			if (p->p_pgrp->pg_id != (pid_t)name[1])
				continue;
			break;

		case KERN_PROC_TTY:
			if ((p->p_flag&SCTTY) == 0 || 
			    p->p_session->s_ttyp == NULL ||
			    p->p_session->s_ttyp->t_dev != (dev_t)name[1])
				continue;
			break;

		case KERN_PROC_UID:
			if (p->p_ucred->cr_uid != (uid_t)name[1])
				continue;
			break;

		case KERN_PROC_RUID:
			if (p->p_cred->p_ruid != (uid_t)name[1])
				continue;
			break;
		}
		if (buflen >= sizeof(struct kinfo_proc)) {
			fill_eproc(p, &eproc);
			if (error = copyout((caddr_t)p, &dp->kp_proc, 
			    sizeof(struct proc)))
				return (error);
			if (error = copyout((caddr_t)&eproc, &dp->kp_eproc, 
			    sizeof(eproc)))
				return (error);
			dp++;
			buflen -= sizeof(struct kinfo_proc);
		}
		needed += sizeof(struct kinfo_proc);
	}
	if (doingzomb == 0) {
		p = zombproc;
		doingzomb++;
		goto again;
	}
	if (where != NULL) {
		*sizep = (caddr_t)dp - where;
		if (needed > *sizep)
			return (ENOMEM);
	} else {
		needed += KERN_PROCSLOP;
		*sizep = needed;
	}
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

#ifdef COMPAT_43
#include <sys/kinfo.h>
#include <sys/socket.h>

struct getkerninfo_args {
	int	op;
	char	*where;
	int	*size;
	int	arg;
};

getkerninfo(p, uap, retval)
	struct proc *p;
	register struct getkerninfo_args *uap;
	int *retval;
{
	int error, name[5];
	u_int size;

	if (error = copyin((caddr_t)uap->size, (caddr_t)&size, sizeof(size)))
		return (error);

	switch (ki_type(uap->op)) {

	case KINFO_RT:
		name[0] = PF_ROUTE;
		name[1] = 0;
		name[2] = (uap->op & 0xff0000) >> 16;
		name[3] = uap->op & 0xff;
		name[4] = uap->arg;
		error = net_sysctl(name, 5, uap->where, &size, NULL, 0);
		break;

	case KINFO_VNODE:
		name[0] = KERN_VNODE;
		error = kern_sysctl(name, 1, uap->where, &size, NULL, 0);
		break;

	case KINFO_PROC:
		name[0] = KERN_PROC;
		name[1] = uap->op & 0xff;
		name[2] = uap->arg;
		error = kern_sysctl(name, 3, uap->where, &size, NULL, 0);
		break;

	case KINFO_FILE:
		name[0] = KERN_FILE;
		error = kern_sysctl(name, 1, uap->where, &size, NULL, 0);
		break;

	case KINFO_METER:
		name[0] = VM_METER;
		error = vm_sysctl(name, 1, uap->where, &size, NULL, 0);
		break;

	case KINFO_LOADAVG:
		name[0] = VM_LOADAVG;
		error = vm_sysctl(name, 1, uap->where, &size, NULL, 0);
		break;

	case KINFO_CLOCKRATE:
		name[0] = KERN_CLOCKRATE;
		error = kern_sysctl(name, 1, uap->where, &size, NULL, 0);
		break;

	default:
		return (EINVAL);
	}
	if (error)
		return (error);
	*retval = size;
	return (copyout((caddr_t)&size, (caddr_t)uap->size, sizeof(size)));
}
#endif /* COMPAT_43 */

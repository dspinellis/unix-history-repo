/*
 * Copyright (c) 1982, 1986, 1989, 1993
 *	The Regents of the University of California.  All rights reserved.
 *
 * %sccs.include.redist.c%
 *
 *	@(#)kern_xxx.c	8.3 (Berkeley) %G%
 */

#include <sys/param.h>
#include <sys/systm.h>
#include <sys/kernel.h>
#include <sys/proc.h>
#include <sys/reboot.h>
#include <vm/vm.h>
#include <sys/sysctl.h>

#include <sys/mount.h>
#include <sys/syscallargs.h>

/* ARGSUSED */
int
reboot(p, uap, retval)
	struct proc *p;
	struct reboot_args /* {
		syscallarg(int) opt;
	} */ *uap;
	register_t *retval;
{
	int error;

	if (error = suser(p->p_ucred, &p->p_acflag))
		return (error);
	boot(SCARG(uap, opt));
	return (0);
}

#if defined(COMPAT_43) || defined(COMPAT_SUNOS)

/* ARGSUSED */
int
compat_43_gethostname(p, uap, retval)
	struct proc *p;
	struct compat_43_gethostname_args /* {
		syscallarg(char *) hostname;
		syscallarg(u_int) len;
	} */ *uap;
	register_t *retval;
{
	int name;

	name = KERN_HOSTNAME;
	return (kern_sysctl(&name, 1, SCARG(uap, hostname), &SCARG(uap, len),
	    0, 0));
}

/* ARGSUSED */
int
compat_43_sethostname(p, uap, retval)
	struct proc *p;
	register struct compat_43_sethostname_args /* {
		syscallarg(char *) hostname;
		syscallarg(u_int) len;
	} */ *uap;
	register_t *retval;
{
	int name;
	int error;

	if (error = suser(p->p_ucred, &p->p_acflag))
		return (error);
	name = KERN_HOSTNAME;
	return (kern_sysctl(&name, 1, 0, 0, SCARG(uap, hostname),
	    SCARG(uap, len)));
}

/* ARGSUSED */
int
compat_43_gethostid(p, uap, retval)
	struct proc *p;
	void *uap;
	register_t *retval;
{

	*(int32_t *)retval = hostid;
	return (0);
}
#endif /* COMPAT_43 || COMPAT_SUNOS */

#ifdef COMPAT_43
/* ARGSUSED */
int
compat_43_sethostid(p, uap, retval)
	struct proc *p;
	struct compat_43_sethostid_args /* {
		syscallarg(int32_t) hostid;
	} */ *uap;
	register_t *retval;
{
	int error;

}

int
compat_43_quota(p, uap, retval)
	struct proc *p;
	void *uap;
	register_t *retval;
{

	return (ENOSYS);
}
#endif /* COMPAT_43 */

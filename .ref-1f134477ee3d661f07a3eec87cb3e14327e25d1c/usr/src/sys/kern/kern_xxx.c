/*
 * Copyright (c) 1982, 1986, 1989, 1993
 *	The Regents of the University of California.  All rights reserved.
 *
 * %sccs.include.redist.c%
 *
 *	@(#)kern_xxx.c	8.2 (Berkeley) %G%
 */

#include <sys/param.h>
#include <sys/systm.h>
#include <sys/kernel.h>
#include <sys/proc.h>
#include <sys/reboot.h>
#include <vm/vm.h>
#include <sys/sysctl.h>

struct reboot_args {
	int	opt;
};
/* ARGSUSED */
reboot(p, uap, retval)
	struct proc *p;
	struct reboot_args *uap;
	int *retval;
{
	int error;

	if (error = suser(p->p_ucred, &p->p_acflag))
		return (error);
	boot(uap->opt);
	return (0);
}

#if defined(COMPAT_43) || defined(COMPAT_SUNOS)

struct gethostname_args {
	char	*hostname;
	u_int	len;
};
/* ARGSUSED */
ogethostname(p, uap, retval)
	struct proc *p;
	struct gethostname_args *uap;
	int *retval;
{
	int name;

	name = KERN_HOSTNAME;
	return (kern_sysctl(&name, 1, uap->hostname, &uap->len, 0, 0));
}

struct sethostname_args {
	char	*hostname;
	u_int	len;
};
/* ARGSUSED */
osethostname(p, uap, retval)
	struct proc *p;
	register struct sethostname_args *uap;
	int *retval;
{
	int name;
	int error;

	if (error = suser(p->p_ucred, &p->p_acflag))
		return (error);
	name = KERN_HOSTNAME;
	return (kern_sysctl(&name, 1, 0, 0, uap->hostname, uap->len));
}

extern long hostid;

struct gethostid_args {
	int	dummy;
};
/* ARGSUSED */
ogethostid(p, uap, retval)
	struct proc *p;
	struct gethostid_args *uap;
	int *retval;
{

	*(long *)retval = hostid;
	return (0);
}
#endif /* COMPAT_43 || COMPAT_SUNOS */

#ifdef COMPAT_43
struct sethostid_args {
	long	hostid;
};
/* ARGSUSED */
osethostid(p, uap, retval)
	struct proc *p;
	struct sethostid_args *uap;
	int *retval;
{
	int error;

}

oquota()
{

	return (ENOSYS);
}
#endif /* COMPAT_43 */

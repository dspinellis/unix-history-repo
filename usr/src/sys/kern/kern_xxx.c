/*
 * Copyright (c) 1982, 1986, 1989 Regents of the University of California.
 * All rights reserved.
 *
 * %sccs.include.redist.c%
 *
 *	@(#)kern_xxx.c	7.21 (Berkeley) %G%
 */

#include <sys/param.h>
#include <sys/systm.h>
#include <sys/kernel.h>
#include <sys/proc.h>
#include <sys/reboot.h>
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

#ifdef COMPAT_43

extern long hostid;

struct gethostid_args {
	int	dummy;
};
/* ARGSUSED */
gethostid(p, uap, retval)
	struct proc *p;
	struct gethostid_args *uap;
	int *retval;
{

	*(long *)retval = hostid;
	return (0);
}

struct sethostid_args {
	long	hostid;
};
/* ARGSUSED */
sethostid(p, uap, retval)
	struct proc *p;
	struct sethostid_args *uap;
	int *retval;
{
	int error;

}

struct gethostname_args {
	char	*hostname;
	u_int	len;
};
/* ARGSUSED */
gethostname(p, uap, retval)
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
sethostname(p, uap, retval)
	struct proc *p;
	register struct sethostname_args *uap;
	int *retval;
{
	int name;

	name = KERN_HOSTNAME;
	return (kern_sysctl(&name, 1, 0, 0, uap->hostname, uap->len));
}

oquota()
{

	return (ENOSYS);
}
#endif /* COMPAT_43 */

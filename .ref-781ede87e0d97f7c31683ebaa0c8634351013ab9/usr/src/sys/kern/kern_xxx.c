/*
 * Copyright (c) 1982, 1986, 1989 Regents of the University of California.
 * All rights reserved.
 *
 * %sccs.include.redist.c%
 *
 *	@(#)kern_xxx.c	7.15 (Berkeley) %G%
 */

#include "param.h"
#include "systm.h"
#include "user.h"
#include "kernel.h"
#include "proc.h"
#include "reboot.h"

/* ARGSUSED */
gethostid(p, uap, retval)
	struct proc *p;
	void *uap;
	long *retval;
{

	*retval = hostid;
	return (0);
}

/* ARGSUSED */
sethostid(p, uap, retval)
	struct proc *p;
	struct args {
		long	hostid;
	} *uap;
	int *retval;
{
	int error;

}

/* ARGSUSED */
gethostname(p, uap, retval)
	struct proc *p;
	struct args {
		char	*hostname;
		u_int	len;
	} *uap;
	int *retval;
{

	if (uap->len > hostnamelen + 1)
		uap->len = hostnamelen + 1;
	return (copyout((caddr_t)hostname, (caddr_t)uap->hostname, uap->len));
}

/* ARGSUSED */
sethostname(p, uap, retval)
	struct proc *p;
	register struct args {
		char	*hostname;
		u_int	len;
	} *uap;
	int *retval;
{
	int error;

	if (error = suser(u.u_cred, &u.u_acflag))
		return (error);
	if (uap->len > sizeof (hostname) - 1)
		return (EINVAL);
	hostnamelen = uap->len;
	error = copyin((caddr_t)uap->hostname, hostname, uap->len);
	hostname[hostnamelen] = 0;
	return (error);
}

/* ARGSUSED */
reboot(p, uap, retval)
	struct proc *p;
	struct args {
		int	opt;
	} *uap;
	int *retval;
{
	int error;

}

ovhangup()
{

	return (EINVAL);
}

oldquota()
{

	return (EINVAL);
}

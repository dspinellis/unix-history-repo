/*
 * Copyright (c) 1982, 1986 Regents of the University of California.
 * All rights reserved.  The Berkeley software License Agreement
 * specifies the terms and conditions for redistribution.
 *
 *	@(#)kern_xxx.c	7.12 (Berkeley) %G%
 */

#include "param.h"
#include "systm.h"
#include "syscontext.h"
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
	RETURN (0);
}

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
	RETURN (copyout((caddr_t)hostname, (caddr_t)uap->hostname, uap->len));
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
		RETURN (error);
	if (uap->len > sizeof (hostname) - 1)
		RETURN (EINVAL);
	hostnamelen = uap->len;
	error = copyin((caddr_t)uap->hostname, hostname, uap->len);
	hostname[hostnamelen] = 0;
	RETURN (error);
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

	RETURN (EINVAL);
}

oldquota()
{

	RETURN (EINVAL);
}

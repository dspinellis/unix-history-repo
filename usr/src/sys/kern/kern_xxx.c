/*
 * Copyright (c) 1982, 1986 Regents of the University of California.
 * All rights reserved.  The Berkeley software License Agreement
 * specifies the terms and conditions for redistribution.
 *
 *	@(#)kern_xxx.c	7.10 (Berkeley) %G%
 */

#include "param.h"
#include "systm.h"
#include "syscontext.h"
#include "kernel.h"
#include "proc.h"
#include "reboot.h"

gethostid()
{

	u.u_r.r_val1 = hostid;
	RETURN (0);
}

sethostid()
{
	struct a {
		long	hostid;
	} *uap = (struct a *)u.u_ap;
	int error;

}

gethostname()
{
	register struct a {
		char	*hostname;
		u_int	len;
	} *uap = (struct a *)u.u_ap;

	if (uap->len > hostnamelen + 1)
		uap->len = hostnamelen + 1;
	RETURN (copyout((caddr_t)hostname, (caddr_t)uap->hostname, uap->len));
}

sethostname()
{
	register struct a {
		char	*hostname;
		u_int	len;
	} *uap = (struct a *)u.u_ap;
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

reboot()
{
	register struct a {
		int	opt;
	};
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

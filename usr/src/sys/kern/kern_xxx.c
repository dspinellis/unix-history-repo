/*
 * Copyright (c) 1982, 1986, 1989 Regents of the University of California.
 * All rights reserved.
 *
 * %sccs.include.redist.c%
 *
 *	@(#)kern_xxx.c	7.20 (Berkeley) %G%
 */

#include <sys/param.h>
#include <sys/systm.h>
#include <sys/kernel.h>
#include <sys/proc.h>
#include <sys/reboot.h>

char	hostname[MAXHOSTNAMELEN];
int	hostnamelen;
long	hostid;

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

	if (uap->len > hostnamelen + 1)
		uap->len = hostnamelen + 1;
	return (copyout((caddr_t)hostname, (caddr_t)uap->hostname, uap->len));
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
	int error;

	if (error = suser(p->p_ucred, &p->p_acflag))
		return (error);
	if (uap->len > sizeof (hostname) - 1)
		return (EINVAL);
	hostnamelen = uap->len;
	error = copyin((caddr_t)uap->hostname, hostname, uap->len);
	hostname[hostnamelen] = 0;
	return (error);
}

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

}

#ifdef COMPAT_43
oquota()
{

	return (ENOSYS);
}
#endif

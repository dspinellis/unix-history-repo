/*
 * Copyright (c) 1982, 1986 Regents of the University of California.
 * All rights reserved.  The Berkeley software License Agreement
 * specifies the terms and conditions for redistribution.
 *
 *	@(#)kern_resource.c	7.9 (Berkeley) %G%
 */

#include "param.h"
#include "user.h"
#include "proc.h"

/*
 * Resource controls and accounting.
 */

getpriority(curp, uap, retval)
	struct proc *curp;
	register struct args {
		int	which;
		int	who;
	} *uap;
	int *retval;
{
	register struct proc *p;
	register int low = PRIO_MAX + 1;

	switch (uap->which) {

	case PRIO_PROCESS:
		if (uap->who == 0)
			p = curp;
		else
			p = pfind(uap->who);
		if (p == 0)
			break;
		low = p->p_nice;
		break;

	case PRIO_PGRP: {
		register struct pgrp *pg;

		if (uap->who == 0)
			pg = curp->p_pgrp;
		else if ((pg = pgfind(uap->who)) == NULL)
			break;
		for (p = pg->pg_mem; p != NULL; p = p->p_pgrpnxt) {
			if (p->p_nice < low)
				low = p->p_nice;
		}
		break;
	}

	case PRIO_USER:
		if (uap->who == 0)
			uap->who = p->p_uid;
		for (p = allproc; p != NULL; p = p->p_nxt) {
			if (p->p_uid == uap->who &&
			    p->p_nice < low)
				low = p->p_nice;
		}
		break;

	default:
		return (EINVAL);
	}
	if (low == PRIO_MAX + 1)
		return (ESRCH);
	*retval = low;
	return (0);
}

/* ARGSUSED */
setpriority(curp, uap, retval)
	struct proc *curp;
	register struct args {
		int	which;
		int	who;
		int	prio;
	} *uap;
	int *retval;
{
	register struct proc *p;
	int found = 0, error = 0;

	switch (uap->which) {

	case PRIO_PROCESS:
		if (uap->who == 0)
			p = curp;
		else
			p = pfind(uap->who);
		if (p == 0)
			break;
		error = donice(curp, p, uap->prio);
		found++;
		break;

	case PRIO_PGRP: {
		register struct pgrp *pg;
		 
		if (uap->who == 0)
			pg = curp->p_pgrp;
		else if ((pg = pgfind(uap->who)) == NULL)
			break;
		for (p = pg->pg_mem; p != NULL; p = p->p_pgrpnxt) {
			error = donice(curp, p, uap->prio);
			found++;
		}
		break;
	}

	case PRIO_USER:
		if (uap->who == 0)
			uap->who = p->p_uid;
		for (p = allproc; p != NULL; p = p->p_nxt)
			if (p->p_uid == uap->who) {
				error = donice(curp, p, uap->prio);
				found++;
			}
		break;

	default:
		return (EINVAL);
	}
	if (found == 0)
		return (ESRCH);
	return (0);
}

donice(curp, chgp, n)
	register struct proc *curp, *chgp;
	register int n;
{

	if (curp->p_uid && curp->p_ruid &&
	    curp->p_uid != chgp->p_uid && curp->p_ruid != chgp->p_uid)
		return (EPERM);
	if (n > PRIO_MAX)
		n = PRIO_MAX;
	if (n < PRIO_MIN)
		n = PRIO_MIN;
	if (n < chgp->p_nice && suser(u.u_cred, &u.u_acflag))
		return (EACCES);
	chgp->p_nice = n;
	(void) setpri(chgp);
	return (0);
}

/* ARGSUSED */
setrlimit(p, uap, retval)
	struct proc *p;
	register struct args {
		u_int	which;
		struct	rlimit *lim;
	} *uap;
	int *retval;
{
	struct rlimit alim;
	register struct rlimit *alimp;
	extern unsigned maxdmap;
	int error;

	if (uap->which >= RLIM_NLIMITS)
		return (EINVAL);
	alimp = &u.u_rlimit[uap->which];
	if (error =
	    copyin((caddr_t)uap->lim, (caddr_t)&alim, sizeof (struct rlimit)))
		return (error);
	if (alim.rlim_cur > alimp->rlim_max || alim.rlim_max > alimp->rlim_max)
		if (error = suser(u.u_cred, &u.u_acflag))
			return (error);
	switch (uap->which) {

	case RLIMIT_DATA:
		if (alim.rlim_cur > maxdmap)
			alim.rlim_cur = maxdmap;
		if (alim.rlim_max > maxdmap)
			alim.rlim_max = maxdmap;
		break;

	case RLIMIT_STACK:
		if (alim.rlim_cur > maxdmap)
			alim.rlim_cur = maxdmap;
		if (alim.rlim_max > maxdmap)
			alim.rlim_max = maxdmap;
		break;
	}
	*alimp = alim;
	if (uap->which == RLIMIT_RSS)
		p->p_maxrss = alim.rlim_cur/NBPG;
	return (0);
}

/* ARGSUSED */
getrlimit(p, uap, retval)
	struct proc *p;
	register struct args {
		u_int	which;
		struct	rlimit *rlp;
	} *uap;
	int *retval;
{

	if (uap->which >= RLIM_NLIMITS)
		return (EINVAL);
	return (copyout((caddr_t)&u.u_rlimit[uap->which], (caddr_t)uap->rlp,
	    sizeof (struct rlimit)));
}

/* ARGSUSED */
getrusage(p, uap, retval)
	register struct proc *p;
	register struct args {
		int	who;
		struct	rusage *rusage;
	} *uap;
	int *retval;
{
	register struct rusage *rup;

	switch (uap->who) {

	case RUSAGE_SELF: {
		int s;

		rup = &u.u_ru;
		s = splclock();
		rup->ru_stime = p->p_stime;
		rup->ru_utime = p->p_utime;
		splx(s);
		break;
	}

	case RUSAGE_CHILDREN:
		rup = &u.u_cru;
		break;

	default:
		return (EINVAL);
	}
	return (copyout((caddr_t)rup, (caddr_t)uap->rusage,
	    sizeof (struct rusage)));
}

ruadd(ru, ru2)
	register struct rusage *ru, *ru2;
{
	register long *ip, *ip2;
	register int i;

	timevaladd(&ru->ru_utime, &ru2->ru_utime);
	timevaladd(&ru->ru_stime, &ru2->ru_stime);
	if (ru->ru_maxrss < ru2->ru_maxrss)
		ru->ru_maxrss = ru2->ru_maxrss;
	ip = &ru->ru_first; ip2 = &ru2->ru_first;
	for (i = &ru->ru_last - &ru->ru_first; i > 0; i--)
		*ip++ += *ip2++;
}

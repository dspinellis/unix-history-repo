/*	kern_resource.c	4.14	82/09/12	*/

#include "../h/param.h"
#include "../h/systm.h"
#include "../h/dir.h"
#include "../h/user.h"
#include "../h/inode.h"
#include "../h/proc.h"
#include "../h/seg.h"
#include "../h/fs.h"
#include "../h/uio.h"
#include "../h/vm.h"

/*
 * Resource controls and accounting.
 */

getpriority()
{
	register struct a {
		int	which;
		int	who;
	} *uap = (struct a *)u.u_ap;
	register struct proc *p;

	u.u_r.r_val1 = NZERO+20;
	u.u_error = ESRCH;
	switch (uap->which) {

	case PRIO_PROCESS:
		if (uap->who == 0)
			p = u.u_procp;
		else
			p = pfind(uap->who);
		if (p == 0)
			return;
		u.u_r.r_val1 = u.u_procp->p_nice;
		u.u_error = 0;
		break;

	case PRIO_PGRP:
		if (uap->who == 0)
			uap->who = u.u_procp->p_pgrp;
		for (p = proc; p < procNPROC; p++) {
			if (p->p_stat == NULL)
				continue;
			if (p->p_pgrp == uap->who &&
			    p->p_nice < u.u_r.r_val1) {
				u.u_r.r_val1 = p->p_nice;
				u.u_error = 0;
			}
		}
		break;

	case PRIO_USER:
		if (uap->who == 0)
			uap->who = u.u_uid;
		for (p = proc; p < procNPROC; p++) {
			if (p->p_stat == NULL)
				continue;
			if (p->p_uid == uap->who &&
			    p->p_nice < u.u_r.r_val1) {
				u.u_r.r_val1 = p->p_nice;
				u.u_error = 0;
			}
		}
		break;

	default:
		u.u_error = EINVAL;
		break;
	}
	u.u_r.r_val1 -= NZERO;
}

setpriority()
{
	register struct a {
		int	which;
		int	who;
		int	prio;
	} *uap = (struct a *)u.u_ap;
	register struct proc *p;

	u.u_error = ESRCH;
	switch (uap->which) {

	case PRIO_PROCESS:
		if (uap->who == 0)
			p = u.u_procp;
		else
			p = pfind(uap->who);
		if (p == 0)
			return;
		donice(p, uap->prio);
		break;

	case PRIO_PGRP:
		if (uap->who == 0)
			uap->who = u.u_procp->p_pgrp;
		for (p = proc; p < procNPROC; p++)
			if (p->p_pgrp == uap->who)
				donice(p, uap->prio);
		break;

	case PRIO_USER:
		if (uap->who == 0)
			uap->who = u.u_uid;
		for (p = proc; p < procNPROC; p++)
			if (p->p_uid == uap->who)
				donice(p, uap->prio);
		break;

	default:
		u.u_error = EINVAL;
		break;
	}
}

donice(p, n)
	register struct proc *p;
	register int n;
{

	if (u.u_uid && u.u_ruid &&
	    u.u_uid != p->p_uid && u.u_ruid != p->p_uid) {
		u.u_error = EACCES;
		return;
	}
	n += NZERO;
	if (n >= 2*NZERO)
		n = 2*NZERO - 1;
	if (n < 0)
		n = 0;
	if (n < p->p_nice && !suser()) {
		u.u_error = EACCES;
		return;
	}
	p->p_nice = n;
	(void) setpri(p);
	if (u.u_error == ESRCH)
		u.u_error = 0;
}

setrlimit()
{
	register struct a {
		u_int	which;
		struct	rlimit *lim;
	} *uap = (struct a *)u.u_ap;
	struct rlimit alim;
	register struct rlimit *alimp;

	if (uap->which >= RLIM_NLIMITS) {
		u.u_error = EINVAL;
		return;
	}
	alimp = &u.u_rlimit[uap->which];
	if (copyin((caddr_t)uap->lim, (caddr_t)&alim, sizeof (struct rlimit))) {
		u.u_error = EFAULT;
		return;
	}
	if (alim.rlim_cur > alimp->rlim_max || alim.rlim_max > alimp->rlim_max)
		if (!suser())
			return;
	switch (uap->which) {

	case RLIMIT_DATA:
		if (alim.rlim_cur > ctob(MAXDSIZ))
			alim.rlim_cur = ctob(MAXDSIZ);
		break;

	case RLIMIT_STACK:
		if (alim.rlim_cur > ctob(MAXSSIZ))
			alim.rlim_cur = ctob(MAXSSIZ);
		break;
	}
	*alimp = alim;
	if (uap->which == RLIMIT_RSS)
		u.u_procp->p_maxrss = alim.rlim_cur/NBPG;
}

getrlimit()
{
	register struct a {
		u_int	which;
		struct	rlimit *rlp;
	} *uap = (struct a *)u.u_ap;

	if (uap->which >= RLIM_NLIMITS) {
		u.u_error = EINVAL;
		return;
	}
	if (copyout((caddr_t)&u.u_rlimit[uap->which], uap->rlp,
	    sizeof (struct rlimit))) {
		u.u_error = EFAULT;
		return;
	}
}

getrusage()
{
	register struct a {
		int	who;
		struct	rusage *rusage;
	} *uap = (struct a *)u.u_ap;
	register struct rusage *rup;

	switch (uap->who) {

	case RUSAGE_SELF:
		rup = &u.u_ru;
		break;

	case RUSAGE_CHILDREN:
		rup = &u.u_cru;
		break;

	default:
		u.u_error = EINVAL;
		return;
	}
	if (copyout((caddr_t)rup, uap->rusage, sizeof (struct rusage))) {
		u.u_error = EFAULT;
		return;
	}
}

ruadd(ru, ru2)
	register struct rusage *ru, *ru2;
{
	register int *ip, *ip2;
	register int i;

	timevaladd(&ru->ru_utime, &ru2->ru_utime);
	timevaladd(&ru->ru_stime, &ru2->ru_stime);
	if (ru->ru_maxrss < ru2->ru_maxrss)
		ru->ru_maxrss = ru2->ru_maxrss;
	ip = &ru->ru_first; ip2 = &ru2->ru_first;
	for (i = &ru->ru_last - &ru->ru_first; i > 0; i--)
		*ip++ += *ip2++;
}

#ifndef NOCOMPAT
onice()
{
	register struct a {
		int	niceness;
	} *uap = (struct a *)u.u_ap;
	register struct proc *p = u.u_procp;

	donice(p, (p->p_nice-NZERO)+uap->niceness);
}

#include "../h/times.h"

otimes()
{
	register struct a {
		struct	tms *tmsb;
	} *uap = (struct a *)u.u_ap;
	struct tms atms;

	atms.tms_utime = scale60(&u.u_ru.ru_utime);
	atms.tms_stime = scale60(&u.u_ru.ru_stime);
	atms.tms_cutime = scale60(&u.u_cru.ru_utime);
	atms.tms_cstime = scale60(&u.u_cru.ru_stime);
	if (copyout((caddr_t)&atms, uap->tmsb, sizeof (atms))) {
		u.u_error = EFAULT;
		return;
	}
}

scale60(tvp)
	register struct timeval *tvp;
{

	return (tvp->tv_sec * 60 + tvp->tv_usec / 16667);
}

#include <vtimes.h>

ovtimes()
{
	register struct a {
		struct	vtimes *par;
		struct	vtimes *chi;
	} *uap = (struct a *)u.u_ap;
	struct vtimes avt;

	if (uap->par) {
		getvtimes(&u.u_ru, &avt);
		if (copyout((caddr_t)&avt, (caddr_t)uap->par, sizeof (avt))) {
			u.u_error = EFAULT;
			return;
		}
	}
	if (uap->chi) {
		getvtimes(&u.u_cru, &avt);
		if (copyout((caddr_t)&avt, (caddr_t)uap->chi, sizeof (avt))) {
			u.u_error = EFAULT;
			return;
		}
	}
}

getvtimes(aru, avt)
	register struct rusage *aru;
	register struct vtimes *avt;
{

	avt->vm_utime = scale60(&aru->ru_utime);
	avt->vm_stime = scale60(&aru->ru_stime);
	avt->vm_idsrss = ((aru->ru_idrss+aru->ru_isrss) / hz) * 60;
	avt->vm_ixrss = aru->ru_ixrss / hz * 60;
	avt->vm_maxrss = aru->ru_maxrss;
	avt->vm_majflt = aru->ru_majflt;
	avt->vm_minflt = aru->ru_minflt;
	avt->vm_nswap = aru->ru_nswap;
	avt->vm_inblk = aru->ru_inblock;
	avt->vm_oublk = aru->ru_oublock;
}

ovlimit()
{

	u.u_error = EACCES;
}

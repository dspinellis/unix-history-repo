/*	kern_prot.c	5.3	82/07/24	*/

/*
 * System calls related to processes and protection
 */

/* NEED ALLOCATION AND PROTECTION MECHANISM FOR PROCESS GROUPS */

#include "../h/param.h"
#include "../h/systm.h"
#include "../h/dir.h"
#include "../h/user.h"
#include "../h/reg.h"
#include "../h/inode.h"
#include "../h/proc.h"
#include "../h/clock.h"
#include "../h/mtpr.h"
#include "../h/timeb.h"
#include "../h/times.h"
#include "../h/reboot.h"
#include "../h/fs.h"
#include "../h/conf.h"
#include "../h/buf.h"
#include "../h/mount.h"
#include "../h/quota.h"

getpid()
{

	u.u_r.r_val1 = u.u_procp->p_pid;
	u.u_r.r_val2 = u.u_procp->p_ppid;
}

getpgrp()
{
	register struct a {
		int	pid;
	} *uap = (struct a *)u.u_ap;
	register struct proc *p;

	if (uap->pid == 0)
		uap->pid = u.u_procp->p_pid;
	p = pfind(uap->pid);
	if (p == 0) {
		u.u_error = ESRCH;
		return;
	}
	u.u_r.r_val1 = p->p_pgrp;
}

getuid()
{

	u.u_r.r_val1 = u.u_ruid;
	u.u_r.r_val2 = u.u_uid;
}

getgid()
{

	u.u_r.r_val1 = u.u_rgid;
	u.u_r.r_val2 = u.u_gid;
}

getgrp()
{
	register struct	a {
		int	*gidset;
	} *uap = (struct a *)u.u_ap;

	if (copyout((caddr_t)u.u_grps, (caddr_t)uap->gidset,
	    sizeof (u.u_grps))) {
		u.u_error = EFAULT;
		return;
	}
}

setpgrp()
{
	register struct proc *p;
	register struct a {
		int	pid;
		int	pgrp;
	} *uap = (struct a *)u.u_ap;

	if (uap->pid == 0)
		uap->pid = u.u_procp->p_pid;
	p = pfind(uap->pid);
	if (p == 0) {
		u.u_error = ESRCH;
		return;
	}
	if (p->p_uid != u.u_uid && u.u_uid && !inferior(p)) {
		u.u_error = EPERM;
		return;
	}
	p->p_pgrp = uap->pgrp;
}

setuid()
{
	register uid;
	register struct a {
		int	uid;
	} *uap;

	uap = (struct a *)u.u_ap;
	uid = uap->uid;
	if (u.u_ruid == uid || u.u_uid == uid || suser()) {
#ifdef QUOTA
		if (u.u_quota->q_uid != uid) {
			qclean();
			qstart(getquota(uid, 0, 0));
		}
#endif
		u.u_uid = uid;
		u.u_procp->p_uid = uid;
		u.u_ruid = uid;
	}
}

setgid()
{
	register gid;
	register struct a {
		int	gid;
	} *uap;

	uap = (struct a *)u.u_ap;
	gid = uap->gid;
	if (u.u_rgid == gid || u.u_gid == gid || suser()) {
		u.u_gid = gid;
		u.u_rgid = gid;
	}
}

setgrp()
{
	register struct	a {
		int	*gidset;
	} *uap = (struct a *)u.u_ap;

	if (suser())
		return;
	if (copyin((caddr_t)uap->gidset, (caddr_t)u.u_grps,
	    sizeof (u.u_grps))) {
		u.u_error = EFAULT;
		return;
	}
}

/* BEGIN DEFUNCT */
osetgrp()
{
	register struct	a {
		int *ngrps;
		int *ogrps;
	} *uap = (struct a *)u.u_ap;
	int thegroups[NGRPS/(sizeof(int)*8)];

	if (uap->ogrps && copyout((caddr_t)u.u_grps, (caddr_t)uap->ogrps,
	    sizeof (thegroups))) {
		u.u_error = EFAULT;
		return;
	}
	if (uap->ngrps == 0)
		return;
	if (copyin((caddr_t)uap->ngrps, (caddr_t)thegroups,
	    sizeof (thegroups))) {
		u.u_error = EFAULT;
		return;
	}
	if (suser())
		bcopy((caddr_t)thegroups, (caddr_t)u.u_grps, sizeof (u.u_grps));
}

/*
 * Pid of zero implies current process.
 * Pgrp -1 is getpgrp system call returning
 * current process group.
 */
osetpgrp()
{
	register struct proc *p;
	register struct a {
		int	pid;
		int	pgrp;
	} *uap;

	uap = (struct a *)u.u_ap;
	if (uap->pid == 0)
		p = u.u_procp;
	else {
		p = pfind(uap->pid);
		if (p == 0) {
			u.u_error = ESRCH;
			return;
		}
	}
	if (uap->pgrp <= 0) {
		u.u_r.r_val1 = p->p_pgrp;
		return;
	}
	if (p->p_uid != u.u_uid && u.u_uid && !inferior(p)) {
		u.u_error = EPERM;
		return;
	}
	p->p_pgrp = uap->pgrp;
}
/* END DEFUNCT */

/*
 * Copyright (c) 1982, 1986, 1989 Regents of the University of California.
 * All rights reserved.
 *
 * Redistribution and use in source and binary forms are permitted
 * provided that the above copyright notice and this paragraph are
 * duplicated in all such forms and that any documentation,
 * advertising materials, and other materials related to such
 * distribution and use acknowledge that the software was developed
 * by the University of California, Berkeley.  The name of the
 * University may not be used to endorse or promote products derived
 * from this software without specific prior written permission.
 * THIS SOFTWARE IS PROVIDED ``AS IS'' AND WITHOUT ANY EXPRESS OR
 * IMPLIED WARRANTIES, INCLUDING, WITHOUT LIMITATION, THE IMPLIED
 * WARRANTIES OF MERCHANTABILITY AND FITNESS FOR A PARTICULAR PURPOSE.
 *
 *	@(#)kern_prot.c	7.6 (Berkeley) %G%
 */

/*
 * System calls related to processes and protection
 */

#include "param.h"
#include "acct.h"
#include "systm.h"
#include "user.h"
#include "proc.h"
#include "timeb.h"
#include "times.h"
#include "reboot.h"
#include "mount.h"
#include "buf.h"
#include "../ufs/quota.h"
#include "malloc.h"
#define GRPSTART 0

#include "machine/reg.h"

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
		p = u.u_procp;
	else if ((p = pfind(uap->pid)) == 0) {
		u.u_error = ESRCH;
		return;
	}
	u.u_r.r_val1 = p->p_pgrp->pg_id;
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

getgroups()
{
	register struct	a {
		u_int	gidsetsize;
		int	*gidset;
	} *uap = (struct a *)u.u_ap;
	register gid_t *gp;
	register int *lp;
	int groups[NGROUPS];

	if (uap->gidsetsize == 0) {
		u.u_r.r_val1 = u.u_ngroups - GRPSTART;
		return;
	}
	if (uap->gidsetsize < u.u_ngroups - GRPSTART) {
		u.u_error = EINVAL;
		return;
	}
	uap->gidsetsize = u.u_ngroups - GRPSTART;
	gp = &u.u_groups[GRPSTART];
	for (lp = groups; lp < &groups[uap->gidsetsize]; )
		*lp++ = *gp++;
	u.u_error = copyout((caddr_t)groups, (caddr_t)uap->gidset,
	    uap->gidsetsize * sizeof (groups[0]));
	if (u.u_error)
		return;
	u.u_r.r_val1 = uap->gidsetsize;
}

setsid()
{
	register struct proc *p = u.u_procp;

	if ((p->p_pgid == p->p_pid) || pgfind(p->p_pid))
		u.u_error = EPERM;
	else {
		pgmv(p, p->p_pid, 1);
		u.u_r.r_val1 = p->p_pid;
	}
	return;
}

/*
 * set process group
 *
 * if target pid != caller's pid
 *	pid must be an inferior
 *	pid must be in same session
 *	pid can't have done an exec
 *	there must exist a pid with pgid in same session 
 * pid must not be session leader
 */
setpgrp()
{
	register struct a {
		int	pid;
		int	pgid;
	} *uap = (struct a *)u.u_ap;
	register struct proc *p;
	register struct pgrp *pgrp;

	if (uap->pid == 0)
		p = u.u_procp;
	else if ((p = pfind(uap->pid)) == 0 || !inferior(p)) {
		u.u_error = ESRCH;
		return;
	}
	else if (p != u.u_procp) { 
		if (p->p_session != u.u_procp->p_session) {
			u.u_error = EPERM;
			return;
		}
		if (p->p_flag&SEXEC) {
			u.u_error = EACCES;
			return;
		}
	}
	if (SESS_LEADER(p)) {
		u.u_error = EPERM;
		return;
	}
	if (uap->pgid == 0)
		uap->pgid = p->p_pid;
	else if ((uap->pgid != p->p_pid) &&
		(((pgrp = pgfind(uap->pgid)) == 0) || 
		   pgrp->pg_mem == NULL ||
	           pgrp->pg_session != u.u_procp->p_session)) {
		u.u_error = EPERM;
		return;
	}
	/*
	 * done checking, now doit
	 */
	pgmv(p, uap->pgid, 0);
}

setreuid()
{
	struct a {
		int	ruid;
		int	euid;
	} *uap;
	register int ruid, euid;

	uap = (struct a *)u.u_ap;
	ruid = uap->ruid;
	if (ruid == -1)
		ruid = u.u_ruid;
		return;
	euid = uap->euid;
	if (euid == -1)
		euid = u.u_uid;
		return;
	/*
	 * Everything's okay, do it.
	 * Copy credentials so other references do not
	 * see our changes.
	 */
#ifdef QUOTA
	if (u.u_quota->q_uid != ruid) {
		qclean();
		qstart(getquota((uid_t)ruid, 0, 0));
	}
#endif
	if (u.u_cred->cr_ref > 1)
		u.u_cred = crcopy(u.u_cred);
	u.u_procp->p_uid = euid;
	u.u_ruid = ruid;
	u.u_uid = euid;
}

setregid()
{
	register struct a {
		int	rgid;
		int	egid;
	} *uap;
	register int rgid, egid;

	uap = (struct a *)u.u_ap;
	rgid = uap->rgid;
	if (rgid == -1)
		rgid = u.u_rgid;
		return;
	egid = uap->egid;
	if (egid == -1)
		egid = u.u_gid;
		return;
	if (u.u_cred->cr_ref > 1)
		u.u_cred = crcopy(u.u_cred);
	u.u_rgid = rgid;
	u.u_gid = egid;
}

setgroups()
{
	register struct	a {
		u_int	gidsetsize;
		int	*gidset;
	} *uap = (struct a *)u.u_ap;
	register gid_t *gp;
	register int *lp;
	int ngrp, groups[NGROUPS];

	if (u.u_error = suser(u.u_cred, &u.u_acflag))
		return;
	ngrp = uap->gidsetsize + GRPSTART;
	if (ngrp > sizeof (u.u_groups) / sizeof (u.u_groups[0])) {
		u.u_error = EINVAL;
		return;
	}
	u.u_error = copyin((caddr_t)uap->gidset, (caddr_t)groups,
	    uap->gidsetsize * sizeof (groups[0]));
	if (u.u_error)
		return;
	gp = &u.u_groups[GRPSTART];
	for (lp = groups; lp < &groups[uap->gidsetsize]; )
		*gp++ = *lp++;
	u.u_ngroups = ngrp;
}

/*
 * Check if gid is a member of the group set.
 */
groupmember(gid, cred)
	gid_t gid;
	register struct ucred *cred;
{
	register gid_t *gp;
	gid_t *egp;

	egp = &(cred->cr_groups[cred->cr_ngroups]);
	for (gp = cred->cr_groups; gp < egp; gp++)
		if (*gp == gid)
			return (1);
	return (0);
}

/*
 * Test if the current user is the super user.
 */
suser(cred, acflag)
	struct ucred *cred;
	short *acflag;
{

	if (cred->cr_uid == 0) {
		if (acflag)
			*acflag |= ASU;
		return (0);
	}
	return (EPERM);
}

/*
 * Allocate a zeroed cred structure.
 */
struct ucred *
crget()
{
	register struct ucred *cr;

	MALLOC(cr, struct ucred *, sizeof(*cr), M_CRED, M_WAITOK);
	bzero((caddr_t)cr, sizeof(*cr));
	cr->cr_ref = 1;
	return(cr);
}

/*
 * Free a cred structure.
 * Throws away space when ref count gets to 0.
 */
crfree(cr)
	struct ucred *cr;
{
	int	s = splimp();

	if (--cr->cr_ref != 0) {
		(void) splx(s);
		return;
	}
	FREE((caddr_t)cr, M_CRED);
	(void) splx(s);
}

/*
 * Copy cred structure to a new one and free the old one.
 */
struct ucred *
crcopy(cr)
	struct ucred *cr;
{
	struct ucred *newcr;

	newcr = crget();
	*newcr = *cr;
	crfree(cr);
	newcr->cr_ref = 1;
	return(newcr);
}

/*
 * Dup cred struct to a new held one.
 */
struct ucred *
crdup(cr)
	struct ucred *cr;
{
	struct ucred *newcr;

	newcr = crget();
	*newcr = *cr;
	newcr->cr_ref = 1;
	return(newcr);
}

/*
 * Get login name of process owner, if available
 */

getlogname()
{
	struct a {
		char	*namebuf;
		u_int	namelen;
	} *uap = (struct a *)u.u_ap;

	if (uap->namelen > sizeof (u.u_logname))
		uap->namelen = sizeof (u.u_logname);
	u.u_error = copyout((caddr_t)u.u_logname, (caddr_t)uap->namebuf,
		uap->namelen);
}

/*
 * Set login name of process owner
 */

setlogname()
{
	struct a {
		char	*namebuf;
		u_int	namelen;
	} *uap = (struct a *)u.u_ap;

	if (u.u_error = suser(u.u_cred, &u.u_acflag))
		return;
	if (uap->namelen > sizeof (u.u_logname) - 1)
		u.u_error = EINVAL;
	else
		u.u_error = copyin((caddr_t)uap->namebuf,
			(caddr_t)u.u_logname, uap->namelen);
}

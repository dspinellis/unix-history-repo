/*
 * Copyright (c) 1982, 1986, 1989, 1990 Regents of the University of California.
 * All rights reserved.
 *
 * %sccs.include.redist.c%
 *
 *	@(#)kern_prot.c	7.16 (Berkeley) %G%
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
#include "malloc.h"

/* ARGSUSED */
getpid(p, uap, retval)
	struct proc *p;
	void *uap;
	int *retval;
{

	*retval = p->p_pid;
#ifdef COMPAT_43
	retval[1] = p->p_ppid;
#endif
	return (0);
}

/* ARGSUSED */
getppid(p, uap, retval)
	struct proc *p;
	void *uap;
	int *retval;
{

	*retval = p->p_ppid;
	return (0);
}

getpgrp(p, uap, retval)
	struct proc *p;
	struct args {
		int	pid;
	} *uap;
	int *retval;
{

	if (uap->pid != 0 && (p = pfind(uap->pid)) == 0)
		return (ESRCH);
	*retval = p->p_pgrp->pg_id;
	return (0);
}

/* ARGSUSED */
getuid(p, uap, retval)
	struct proc *p;
	void *uap;
	int *retval;
{

	*retval = p->p_ruid;
#ifdef COMPAT_43
	retval[1] = u.u_cred->cr_uid;
#endif
	return (0);
}

/* ARGSUSED */
geteuid(p, uap, retval)
	struct proc *p;
	void *uap;
	int *retval;
{

	*retval = u.u_cred->cr_uid;
	return (0);
}

/* ARGSUSED */
getgid(p, uap, retval)
	struct proc *p;
	void *uap;
	int *retval;
{

	*retval = p->p_rgid;
#ifdef COMPAT_43
	retval[1] = u.u_cred->cr_groups[0];
#endif
	return (0);
}

/*
 * Get effective group ID.
 * The "egid" is groups[0], and thus could be obtained via getgroups;
 * this is somewhat painful to do correctly in a library function,
 * this the existence of this syscall.
 */
/* ARGSUSED */
getegid(p, uap, retval)
	struct proc *p;
	void *uap;
	int *retval;
{

	*retval = u.u_cred->cr_groups[0];
	return (0);
}

getgroups(p, uap, retval)
	struct proc *p;
	register struct	arg {
		u_int	gidsetsize;
		int	*gidset;		/* XXX not yet POSIX */
	} *uap;
	int *retval;
{
	register gid_t *gp;
	register int *lp;
	register u_int ngrp;
	int groups[NGROUPS];
	int error;

	if ((ngrp = uap->gidsetsize) == 0) {
		*retval = u.u_cred->cr_ngroups;
		return (0);
	}
	if (ngrp < u.u_cred->cr_ngroups)
		return (EINVAL);
	ngrp = u.u_cred->cr_ngroups;
	for (gp = u.u_cred->cr_groups, lp = groups; lp < &groups[ngrp]; )
		*lp++ = *gp++;
	if (error = copyout((caddr_t)groups, (caddr_t)uap->gidset,
	    ngrp * sizeof (groups[0])))
		return (error);
	*retval = ngrp;
	return (0);
}

/* ARGSUSED */
setsid(p, uap, retval)
	struct proc *p;
	void *uap;
	int *retval;
{

	if (p->p_pgid == p->p_pid || pgfind(p->p_pid)) {
		return (EPERM);
	} else {
		pgmv(p, p->p_pid, 1);
		*retval = p->p_pid;
		return (0);
	}
}

/*
 * set process group (setpgrp/setpgid)
 *
 * caller does setpgrp(pid, pgid)
 *
 * pid must be caller or child of caller (ESRCH)
 * if a child
 *	pid must be in same session (EPERM)
 *	pid can't have done an exec (EACCES)
 * if pgid != pid
 * 	there must exist some pid in same session having pgid (EPERM)
 * pid must not be session leader (EPERM)
 */
/* ARGSUSED */
setpgrp(cp, uap, retval)
	struct proc *cp;
	register struct args {
		int	pid;
		int	pgid;
	} *uap;
	int *retval;
{
	register struct proc *p;
	register struct pgrp *pgrp;

	if (uap->pid != 0) {
		if ((p = pfind(uap->pid)) == 0 || !inferior(p))
			return (ESRCH);
		if (p->p_session != cp->p_session)
			return (EPERM);
		if (p->p_flag&SEXEC)
			return (EACCES);
	} else
		p = cp;
	if (SESS_LEADER(p))
		return (EPERM);
	if (uap->pgid == 0)
		uap->pgid = p->p_pid;
	else if ((uap->pgid != p->p_pid) &&
		(((pgrp = pgfind(uap->pgid)) == 0) || 
		   pgrp->pg_mem == NULL ||
	           pgrp->pg_session != u.u_procp->p_session))
		return (EPERM);
	/*
	 * done checking, now do it
	 */
	pgmv(p, uap->pgid, 0);
	return (0);
}

/* ARGSUSED */
setuid(p, uap, retval)
	register struct proc *p;
	struct args {
		int	uid;
	} *uap;
	int *retval;
{
	register uid_t uid;
	int error;

	uid = uap->uid;
	if (uid != p->p_ruid && (error = suser(u.u_cred, &u.u_acflag)))
		return (error);
	/*
	 * Everything's okay, do it.
	 * Copy credentials so other references do not
	 * see our changes.
	 */
	if (u.u_cred->cr_ref > 1)
		u.u_cred = crcopy(u.u_cred);
	u.u_cred->cr_uid = uid;
	p->p_uid = uid;
	p->p_ruid = uid;
	p->p_svuid = uid;
	return (0);
}

/* ARGSUSED */
seteuid(p, uap, retval)
	register struct proc *p;
	struct args {
		int	euid;
	} *uap;
	int *retval;
{
	register uid_t euid;
	int error;

	euid = uap->euid;
	if (euid != p->p_ruid && euid != p->p_svuid &&
	    (error = suser(u.u_cred, &u.u_acflag)))
		return (error);
	/*
	 * Everything's okay, do it.
	 * Copy credentials so other references do not
	 * see our changes.
	 */
	if (u.u_cred->cr_ref > 1)
		u.u_cred = crcopy(u.u_cred);
	u.u_cred->cr_uid = euid;
	p->p_uid = euid;
	return (0);
}

/* ARGSUSED */
setgid(p, uap, retval)
	struct proc *p;
	struct args {
		int	gid;
	} *uap;
	int *retval;
{
	register gid_t gid;
	int error;

	gid = uap->gid;
	if (gid != p->p_rgid && (error = suser(u.u_cred, &u.u_acflag)))
		return (error);
	if (u.u_cred->cr_ref > 1)
		u.u_cred = crcopy(u.u_cred);
	p->p_rgid = gid;
	u.u_cred->cr_groups[0] = gid;
	p->p_svgid = gid;		/* ??? */
	return (0);
}

/* ARGSUSED */
setegid(p, uap, retval)
	struct proc *p;
	struct args {
		int	egid;
	} *uap;
	int *retval;
{
	register gid_t egid;
	int error;

	egid = uap->egid;
	if (egid != p->p_rgid && egid != p->p_svgid &&
	    (error = suser(u.u_cred, &u.u_acflag)))
		return (error);
	if (u.u_cred->cr_ref > 1)
		u.u_cred = crcopy(u.u_cred);
	u.u_cred->cr_groups[0] = egid;
	return (0);
}

#ifdef COMPAT_43
/* ARGSUSED */
osetreuid(p, uap, retval)
	register struct proc *p;
	struct args {
		int	ruid;
		int	euid;
	} *uap;
	int *retval;
{
	register uid_t ruid, euid;
	int error;

	if (uap->ruid == -1)
	    (error = suser(u.u_cred, &u.u_acflag)))
		return (error);
	if (uap->euid == -1)
	    euid != p->p_svuid && (error = suser(u.u_cred, &u.u_acflag)))
		return (error);
	/*
	 * Everything's okay, do it.
	 * Copy credentials so other references do not
	 * see our changes.
	 */
	if (u.u_cred->cr_ref > 1)
		u.u_cred = crcopy(u.u_cred);
	u.u_cred->cr_uid = euid;
	p->p_uid = euid;
	p->p_ruid = ruid;
	return (0);
}

/* ARGSUSED */
osetregid(p, uap, retval)
	struct proc *p;
	struct args {
		int	rgid;
		int	egid;
	} *uap;
	int *retval;
{
	register gid_t rgid, egid;
	int error;

	if (uap->rgid == -1)
	    (error = suser(u.u_cred, &u.u_acflag)))
		return (error);
	if (uap->egid == -1)
	    egid != p->p_svgid && (error = suser(u.u_cred, &u.u_acflag)))
		return (error);
	if (u.u_cred->cr_ref > 1)
		u.u_cred = crcopy(u.u_cred);
	p->p_rgid = rgid;
	u.u_cred->cr_groups[0] = egid;
	return (0);
}
#endif

/* ARGSUSED */
setgroups(p, uap, retval)
	struct proc *p;
	struct args {
		u_int	gidsetsize;
		int	*gidset;
	} *uap;
	int *retval;
{
	register gid_t *gp;
	register u_int ngrp;
	register int *lp;
	int error, groups[NGROUPS];

	if (error = suser(u.u_cred, &u.u_acflag))
		return (error);
	if ((ngrp = uap->gidsetsize) > NGROUPS)
		return (EINVAL);
	if (error = copyin((caddr_t)uap->gidset, (caddr_t)groups,
	    ngrp * sizeof (groups[0])))
		return (error);
	u.u_cred->cr_ngroups = ngrp;
	/* convert from int's to gid_t's */
	for (gp = u.u_cred->cr_groups, lp = groups; ngrp--; )
		*gp++ = *lp++;
	return (0);
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
	return (cr);
}

/*
 * Free a cred structure.
 * Throws away space when ref count gets to 0.
 */
crfree(cr)
	struct ucred *cr;
{
	int s = splimp();

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
	return (newcr);
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
	return (newcr);
}

/*
 * Get login name, if available.
 */
/* ARGSUSED */
getlogin(p, uap, retval)
	struct proc *p;
	struct args {
		char	*namebuf;
		u_int	namelen;
	} *uap;
	int *retval;
{

	if (uap->namelen > sizeof (p->p_logname))
		uap->namelen = sizeof (p->p_logname);
	return (copyout((caddr_t)p->p_logname, (caddr_t)uap->namebuf,
	    uap->namelen));
}

/*
 * Set login name.
 */
/* ARGSUSED */
setlogin(p, uap, retval)
	struct proc *p;
	struct args {
		char	*namebuf;
	} *uap;
	int *retval;
{
	int error;

	if (error = suser(u.u_cred, &u.u_acflag))
		return (error);
	error = copyinstr((caddr_t)uap->namebuf, (caddr_t)p->p_logname,
	    sizeof (p->p_logname) - 1, (int *) 0);
	if (error == ENAMETOOLONG)		/* name too long */
		error = EINVAL;
	return (error);
}

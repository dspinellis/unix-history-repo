/*
 * Copyright (c) 1982, 1986 Regents of the University of California.
 * All rights reserved.  The Berkeley software License Agreement
 * specifies the terms and conditions for redistribution.
 *
 *	@(#)kern_prot.c	7.25 (Berkeley) %G%
 */

/*
 * System calls related to processes and protection
 */

#include "param.h"
#include "systm.h"
#include "dir.h"
#include "ucred.h"
#include "inode.h"
#include "proc.h"
#include "timeb.h"
#include "times.h"

struct args {
	int	dummy;
};

/* ARGSUSED */
getpid(p, uap, retval)
	struct proc *p;
	struct args *uap;
	int *retval;
{

	*retval = p->p_pid;
#if defined(COMPAT_43) || defined(COMPAT_SUNOS)
	retval[1] = p->p_pptr->p_pid;
#endif
	return (0);
}

/* ARGSUSED */
getppid(p, uap, retval)
	struct proc *p;
	struct args *uap;
	int *retval;
{

	*retval = p->p_pptr->p_pid;
	return (0);
}

/* Get process group ID; note that POSIX getpgrp takes no parameter */
getpgrp(p, uap, retval)
	struct proc *p;
	struct args *uap;
	int *retval;
{

	*retval = p->p_pgrp->pg_id;
	return (0);
}

/* ARGSUSED */
getuid(p, uap, retval)
	struct proc *p;
	struct args *uap;
	int *retval;
{

	*retval = p->p_cred->p_ruid;
#if defined(COMPAT_43) || defined(COMPAT_SUNOS)
	retval[1] = p->p_ucred->cr_uid;
#endif
	return (0);
}

/* ARGSUSED */
geteuid(p, uap, retval)
	struct proc *p;
	struct args *uap;
	int *retval;
{

	*retval = p->p_ucred->cr_uid;
	return (0);
}

/* ARGSUSED */
getgid(p, uap, retval)
	struct proc *p;
	struct args *uap;
	int *retval;
{

	*retval = p->p_cred->p_rgid;
#if defined(COMPAT_43) || defined(COMPAT_SUNOS)
	retval[1] = p->p_ucred->cr_groups[0];
#endif
	return (0);
}

/*
 * Get effective group ID.  The "egid" is groups[0], and could be obtained
 * via getgroups.  This syscall exists because it is somewhat painful to do
 * correctly in a library function.
 */
/* ARGSUSED */
getegid(p, uap, retval)
	struct proc *p;
	struct args *uap;
	int *retval;
{

	*retval = p->p_ucred->cr_groups[0];
	return (0);
}

struct getgroups_args {
	u_int	gidsetsize;
	int	*gidset;		/* XXX not yet POSIX */
};
getgroups(p, uap, retval)
	struct proc *p;
	register struct	getgroups_args *uap;
	int *retval;
{
	register struct pcred *pc = p->p_cred;
	register gid_t *gp;
	register int *lp;
	register u_int ngrp;
	int groups[NGROUPS];
	int error;

	for (gp = &u.u_groups[NGROUPS]; gp > u.u_groups; gp--)
		if (gp[-1] != NOGROUP)
			break;
	if (uap->gidsetsize < gp - u.u_groups) {
	if (ngrp < pc->pc_ucred->cr_ngroups)
		return (EINVAL);
	uap->gidsetsize = gp - u.u_groups;
	for (lp = groups, gp = u.u_groups; lp < &groups[uap->gidsetsize]; )
		*lp++ = *gp++;
	if (error = copyout((caddr_t)groups, (caddr_t)uap->gidset,
	    ngrp * sizeof (groups[0])))
		return (error);
	*retval = ngrp;
	return (0);
}

/* ARGSUSED */
setsid(p, uap, retval)
	register struct proc *p;
	struct args *uap;
	int *retval;
{

	if (p->p_pgid == p->p_pid || pgfind(p->p_pid)) {
		return (EPERM);
	} else {
		enterpgrp(p, p->p_pid, 1);
		*retval = p->p_pid;
		return (0);
	}
}

/*
 * set process group (setpgid/old setpgrp)
 *
 * caller does setpgid(targpid, targpgid)
 *
 * pid must be caller or child of caller (ESRCH)
 * if a child
 *	pid must be in same session (EPERM)
 *	pid can't have done an exec (EACCES)
 * if pgid != pid
 * 	there must exist some pid in same session having pgid (EPERM)
 * pid must not be session leader (EPERM)
 */
struct setpgid_args {
	int	pid;	/* target process id */
	int	pgid;	/* target pgrp id */
};
/* ARGSUSED */
setpgid(curp, uap, retval)
	struct proc *curp;
	register struct setpgid_args *uap;
	int *retval;
{
	register struct proc *targp;		/* target process */
	register struct pgrp *pgrp;		/* target pgrp */

	if (uap->pid != 0 && uap->pid != curp->p_pid) {
		if ((targp = pfind(uap->pid)) == 0 || !inferior(targp))
			return (ESRCH);
		if (targp->p_session != curp->p_session)
			return (EPERM);
		if (targp->p_flag&SEXEC)
			return (EACCES);
	} else
		targp = curp;
	if (SESS_LEADER(targp))
		return (EPERM);
	if (uap->pgid == 0)
		uap->pgid = targp->p_pid;
	else if (uap->pgid != targp->p_pid)
		if ((pgrp = pgfind(uap->pgid)) == 0 ||
	            pgrp->pg_session != curp->p_session)
			return (EPERM);
	enterpgrp(targp, uap->pgid, 0);
	return (0);
}

struct setuid_args {
	int	uid;
};
/* ARGSUSED */
setuid(p, uap, retval)
	struct proc *p;
	struct setuid_args *uap;
	int *retval;
{
	register struct pcred *pc = p->p_cred;
	register uid_t uid;
	int error;

	uid = uap->uid;
	if (uid != pc->p_ruid &&
	    (error = suser(pc->pc_ucred, &p->p_acflag)))
		return (error);
	/*
	 * Everything's okay, do it.  Copy credentials so other references do
	 * not see our changes.
	 */
	pc->pc_ucred = crcopy(pc->pc_ucred);
	pc->pc_ucred->cr_uid = uid;
	pc->p_ruid = uid;
	pc->p_svuid = uid;
	p->p_flag |= SUGID;
	return (0);
}

struct seteuid_args {
	int	euid;
};
/* ARGSUSED */
seteuid(p, uap, retval)
	struct proc *p;
	struct seteuid_args *uap;
	int *retval;
{
	register struct pcred *pc = p->p_cred;
	register uid_t euid;
	int error;

	euid = uap->euid;
	if (euid != pc->p_ruid && euid != pc->p_svuid &&
	    (error = suser(pc->pc_ucred, &p->p_acflag)))
		return (error);
	/*
	 * Everything's okay, do it.  Copy credentials so other references do
	 * not see our changes.
	 */
	pc->pc_ucred = crcopy(pc->pc_ucred);
	pc->pc_ucred->cr_uid = euid;
	p->p_flag |= SUGID;
	return (0);
}

struct setgid_args {
	int	gid;
};
/* ARGSUSED */
setgid(p, uap, retval)
	struct proc *p;
	struct setgid_args *uap;
	int *retval;
{
	register struct pcred *pc = p->p_cred;
	register gid_t gid;
	int error;

	gid = uap->gid;
	if (gid != pc->p_rgid && (error = suser(pc->pc_ucred, &p->p_acflag)))
		return (error);
	pc->pc_ucred = crcopy(pc->pc_ucred);
	pc->pc_ucred->cr_groups[0] = gid;
	pc->p_rgid = gid;
	pc->p_svgid = gid;		/* ??? */
	p->p_flag |= SUGID;
	return (0);
}

struct setegid_args {
	int	egid;
};
/* ARGSUSED */
setegid(p, uap, retval)
	struct proc *p;
	struct setegid_args *uap;
	int *retval;
{
	register struct pcred *pc = p->p_cred;
	register gid_t egid;
	int error;

	egid = uap->egid;
	if (egid != pc->p_rgid && egid != pc->p_svgid &&
	    (error = suser(pc->pc_ucred, &p->p_acflag)))
		return (error);
	pc->pc_ucred = crcopy(pc->pc_ucred);
	pc->pc_ucred->cr_groups[0] = egid;
	p->p_flag |= SUGID;
	return (0);
}

struct setgroups_args {
	u_int	gidsetsize;
	int	*gidset;
};
/* ARGSUSED */
setgroups(p, uap, retval)
	struct proc *p;
	struct setgroups_args *uap;
	int *retval;
{
	register struct pcred *pc = p->p_cred;
	register gid_t *gp;
	register u_int ngrp;
	register int *lp;
	int groups[NGROUPS];

	if (error = suser(pc->pc_ucred, &p->p_acflag))
		return (error);
	if (uap->gidsetsize > sizeof (u.u_groups) / sizeof (u.u_groups[0])) {
	if ((ngrp = uap->gidsetsize) > NGROUPS)
		return (EINVAL);
	if (error = copyin((caddr_t)uap->gidset, (caddr_t)groups,
	    ngrp * sizeof (groups[0])))
		return (error);
}

/*
 * Group utility functions.
 */

/*
 * Delete gid from the group set.
 */
leavegroup(gid)
	gid_t gid;
{
	register gid_t *gp;

	for (gp = u.u_groups; gp < &u.u_groups[NGROUPS]; gp++)
		if (*gp == gid)
			goto found;
	return;
found:
	for (; gp < &u.u_groups[NGROUPS-1]; gp++)
		*gp = *(gp+1);
	*gp = NOGROUP;
}

/*
 * Add gid to the group set.
 */
entergroup(gid)
	gid_t gid;
{
	register gid_t *gp;
	for (gp = u.u_groups; gp < &u.u_groups[NGROUPS]; gp++) {
		if (*gp == gid)
			return (0);
		if (*gp == NOGROUP) {
			*gp = gid;
			return (0);
		}
	}
	return (-1);
}

/*
 * Check if gid is a member of the group set.
 */
groupmember(gid)
	gid_t gid;
{
	register gid_t *gp;

	if (u.u_gid == gid)
		return (1);
	for (gp = u.u_groups; gp < &u.u_groups[NGROUPS] && *gp != NOGROUP; gp++)
		if (*gp == gid)
			return (1);
	return (0);
}

/*
 * Get login name, if available.
 */
struct getlogin_args {
	char	*namebuf;
	u_int	namelen;
};
/* ARGSUSED */
getlogin(p, uap, retval)
	struct proc *p;
	struct getlogin_args *uap;
	int *retval;
{

	if (uap->namelen > sizeof (p->p_pgrp->pg_session->s_login))
		uap->namelen = sizeof (p->p_pgrp->pg_session->s_login);
	return (copyout((caddr_t) p->p_pgrp->pg_session->s_login,
	    (caddr_t) uap->namebuf, uap->namelen));
}

/*
 * Set login name.
 */
struct setlogin_args {
	char	*namebuf;
};
/* ARGSUSED */
setlogin(p, uap, retval)
	struct proc *p;
	struct setlogin_args *uap;
	int *retval;
{
	int error;

	if (error = suser(p->p_ucred, &p->p_acflag))
		return (error);
	error = copyinstr((caddr_t) uap->namebuf,
	    (caddr_t) p->p_pgrp->pg_session->s_login,
	    sizeof (p->p_pgrp->pg_session->s_login) - 1, (u_int *)0);
	if (error == ENAMETOOLONG)
		error = EINVAL;
	return (error);
}

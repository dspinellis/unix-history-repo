/*
 * Copyright (c) 1982, 1986 Regents of the University of California.
 * All rights reserved.  The Berkeley software License Agreement
 * specifies the terms and conditions for redistribution.
 *
 *	@(#)kern_prot.c	8.9 (Berkeley) %G%
 */

/*
 * System calls related to processes and protection
 */


#include <sys/mount.h>
#include <sys/syscallargs.h>

/* ARGSUSED */
int
getpid(p, uap, retval)
	struct proc *p;
	void *uap;
	register_t *retval;
{

	*retval = p->p_pid;
#if defined(COMPAT_43) || defined(COMPAT_SUNOS)
	retval[1] = p->p_pptr->p_pid;
#endif
	return (0);
}

/* ARGSUSED */
int
getppid(p, uap, retval)
	struct proc *p;
	void *uap;
	register_t *retval;
{

	*retval = p->p_pptr->p_pid;
	return (0);
}

/* Get process group ID; note that POSIX getpgrp takes no parameter */
int
getpgrp(p, uap, retval)
	struct proc *p;
	void *uap;
	register_t *retval;
{

	*retval = p->p_pgrp->pg_id;
	return (0);
}

/* ARGSUSED */
int
getuid(p, uap, retval)
	struct proc *p;
	void *uap;
	register_t *retval;
{

	*retval = p->p_cred->p_ruid;
#if defined(COMPAT_43) || defined(COMPAT_SUNOS)
	retval[1] = p->p_ucred->cr_uid;
#endif
	return (0);
}

/* ARGSUSED */
int
geteuid(p, uap, retval)
	struct proc *p;
	void *uap;
	register_t *retval;
{

	*retval = p->p_ucred->cr_uid;
	return (0);
}

/* ARGSUSED */
int
getgid(p, uap, retval)
	struct proc *p;
	void *uap;
	register_t *retval;
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
int
getegid(p, uap, retval)
	struct proc *p;
	void *uap;
	register_t *retval;
{

	*retval = p->p_ucred->cr_groups[0];
	return (0);
}

int
getgroups(p, uap, retval)
	struct proc *p;
	register struct getgroups_args /* {
		syscallarg(u_int) gidsetsize;
		syscallarg(gid_t *) gidset;
	} */ *uap;
	register_t *retval;
{
	register struct pcred *pc = p->p_cred;
	register u_int ngrp;
	int error;

	for (gp = &u.u_groups[NGROUPS]; gp > u.u_groups; gp--)
		if (gp[-1] != NOGROUP)
			break;
	if (uap->gidsetsize < gp - u.u_groups) {
	if (ngrp < pc->pc_ucred->cr_ngroups)
		return (EINVAL);
	uap->gidsetsize = gp - u.u_groups;
	for (lp = groups, gp = u.u_groups; lp < &groups[uap->gidsetsize]; )
	if (error = copyout((caddr_t)pc->pc_ucred->cr_groups,
	    (caddr_t)SCARG(uap, gidset), ngrp * sizeof(gid_t)))
		return (error);
	*retval = ngrp;
	return (0);
}

/* ARGSUSED */
int
setsid(p, uap, retval)
	register struct proc *p;
	void *uap;
	register_t *retval;
{

	if (p->p_pgid == p->p_pid || pgfind(p->p_pid)) {
		return (EPERM);
	} else {
		(void)enterpgrp(p, p->p_pid, 1);
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
/* ARGSUSED */
int
setpgid(curp, uap, retval)
	struct proc *curp;
	register struct setpgid_args /* {
		syscallarg(int) pid;
		syscallarg(int) pgid;
	} */ *uap;
	register_t *retval;
{
	register struct proc *targp;		/* target process */
	register struct pgrp *pgrp;		/* target pgrp */

	if (SCARG(uap, pid) != 0 && SCARG(uap, pid) != curp->p_pid) {
		if ((targp = pfind(SCARG(uap, pid))) == 0 || !inferior(targp))
			return (ESRCH);
		if (targp->p_session != curp->p_session)
			return (EPERM);
		if (targp->p_flag & P_EXEC)
			return (EACCES);
	} else
		targp = curp;
	if (SESS_LEADER(targp))
		return (EPERM);
	if (SCARG(uap, pgid) == 0)
		SCARG(uap, pgid) = targp->p_pid;
	else if (SCARG(uap, pgid) != targp->p_pid)
		if ((pgrp = pgfind(SCARG(uap, pgid))) == 0 ||
	            pgrp->pg_session != curp->p_session)
			return (EPERM);
	return (enterpgrp(targp, SCARG(uap, pgid), 0));
}

/* ARGSUSED */
int
setuid(p, uap, retval)
	struct proc *p;
	struct setuid_args /* {
		syscallarg(uid_t) uid;
	} */ *uap;
	register_t *retval;
{
	register struct pcred *pc = p->p_cred;
	register uid_t uid;
	int error;

	uid = SCARG(uap, uid);
	if (uid != pc->p_ruid &&
	    (error = suser(pc->pc_ucred, &p->p_acflag)))
		return (error);
	/*
	 * Everything's okay, do it.
	 * Transfer proc count to new user.
	 * Copy credentials so other references do not see our changes.
	 */
	(void)chgproccnt(pc->p_ruid, -1);
	(void)chgproccnt(uid, 1);
	pc->pc_ucred = crcopy(pc->pc_ucred);
	pc->pc_ucred->cr_uid = uid;
	pc->p_ruid = uid;
	pc->p_svuid = uid;
	p->p_flag |= P_SUGID;
	return (0);
}

/* ARGSUSED */
int
seteuid(p, uap, retval)
	struct proc *p;
	struct seteuid_args /* {
		syscallarg(uid_t) euid;
	} */ *uap;
	register_t *retval;
{
	register struct pcred *pc = p->p_cred;
	register uid_t euid;
	int error;

	euid = SCARG(uap, euid);
	if (euid != pc->p_ruid && euid != pc->p_svuid &&
	    (error = suser(pc->pc_ucred, &p->p_acflag)))
		return (error);
	/*
	 * Everything's okay, do it.  Copy credentials so other references do
	 * not see our changes.
	 */
	pc->pc_ucred = crcopy(pc->pc_ucred);
	pc->pc_ucred->cr_uid = euid;
	p->p_flag |= P_SUGID;
	return (0);
}

/* ARGSUSED */
int
setgid(p, uap, retval)
	struct proc *p;
	struct setgid_args /* {
		syscallarg(gid_t) gid;
	} */ *uap;
	register_t *retval;
{
	register struct pcred *pc = p->p_cred;
	register gid_t gid;
	int error;

	gid = SCARG(uap, gid);
	if (gid != pc->p_rgid && (error = suser(pc->pc_ucred, &p->p_acflag)))
		return (error);
	pc->pc_ucred = crcopy(pc->pc_ucred);
	pc->pc_ucred->cr_groups[0] = gid;
	pc->p_rgid = gid;
	pc->p_svgid = gid;		/* ??? */
	p->p_flag |= P_SUGID;
	return (0);
}

/* ARGSUSED */
int
setegid(p, uap, retval)
	struct proc *p;
	struct setegid_args /* {
		syscallarg(gid_t) egid;
	} */ *uap;
	register_t *retval;
{
	register struct pcred *pc = p->p_cred;
	register gid_t egid;
	int error;

	egid = SCARG(uap, egid);
	if (egid != pc->p_rgid && egid != pc->p_svgid &&
	    (error = suser(pc->pc_ucred, &p->p_acflag)))
		return (error);
	pc->pc_ucred = crcopy(pc->pc_ucred);
	pc->pc_ucred->cr_groups[0] = egid;
	p->p_flag |= P_SUGID;
	return (0);
}

/* ARGSUSED */
int
setgroups(p, uap, retval)
	struct proc *p;
	struct setgroups_args /* {
		syscallarg(u_int) gidsetsize;
		syscallarg(gid_t *) gidset;
	} */ *uap;
	register_t *retval;
{
	register struct pcred *pc = p->p_cred;
	register u_int ngrp;

	if (error = suser(pc->pc_ucred, &p->p_acflag))
		return (error);
	if (uap->gidsetsize > sizeof (u.u_groups) / sizeof (u.u_groups[0])) {
	ngrp = SCARG(uap, gidsetsize);
	if (ngrp < 1 || ngrp > NGROUPS)
		return (EINVAL);
}

#if defined(COMPAT_43) || defined(COMPAT_SUNOS)
/* ARGSUSED */
int
compat_43_setreuid(p, uap, retval)
	register struct proc *p;
	struct compat_43_setreuid_args /* {
		syscallarg(int) ruid;
		syscallarg(int) euid;
	} */ *uap;
	register_t *retval;
{
	register struct pcred *pc = p->p_cred;
	union {
		struct setuid_args sa;
		struct seteuid_args ea;
	} args;

	/*
	 * If ruid == euid then setreuid is being used to emulate setuid,
	 * just do it.
	 */
	if (SCARG(uap, ruid) != -1 && SCARG(uap, ruid) == SCARG(uap, euid)) {
		SCARG(&args.sa, uid) = SCARG(uap, ruid);
		return (setuid(p, &args.sa, retval));
	}
	/*
	 * Otherwise we assume that the intent of setting ruid is to be
	 * able to get back ruid priviledge (i.e. swapping ruid and euid).
	 * So we make sure that we will be able to do so, but do not
	 * actually set the ruid.
	 */
	if (SCARG(uap, ruid) != (uid_t)-1 && SCARG(uap, ruid) != pc->p_ruid &&
	    SCARG(uap, ruid) != pc->p_svuid)
		return (EPERM);
	if (SCARG(uap, euid) == (uid_t)-1)
		return (0);
	SCARG(&args.ea, euid) = SCARG(uap, euid);
	return (seteuid(p, &args.ea, retval));
}

/* ARGSUSED */
int
compat_43_setregid(p, uap, retval)
	register struct proc *p;
	struct compat_43_setregid_args /* {
		syscallarg(int) rgid;
		syscallarg(int) egid;
	} */ *uap;
	register_t *retval;
{
	register struct pcred *pc = p->p_cred;
	union {
		struct setgid_args sa;
		struct setegid_args ea;
	} args;

	/*
	 * If rgid == egid then setreuid is being used to emulate setgid,
	 * just do it.
	 */
	if (SCARG(uap, rgid) != -1 && SCARG(uap, rgid) == SCARG(uap, egid)) {
		SCARG(&args.sa, gid) = SCARG(uap, rgid);
		return (setgid(p, &args.sa, retval));
	}
	/*
	 * Otherwise we assume that the intent of setting rgid is to be
	 * able to get back rgid priviledge (i.e. swapping rgid and egid).
	 * So we make sure that we will be able to do so, but do not
	 * actually set the rgid.
	 */
	if (SCARG(uap, rgid) != (gid_t)-1 && SCARG(uap, rgid) != pc->p_rgid &&
	    SCARG(uap, rgid) != pc->p_svgid)
		return (EPERM);
	if (SCARG(uap, egid) == (gid_t)-1)
		return (0);
	SCARG(&args.ea, egid) = SCARG(uap, egid);
	return (setegid(p, &args.ea, retval));
}
#endif /* defined(COMPAT_43) || defined(COMPAT_SUNOS) */

/*
 * Group utility functions.
 */
int

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
int
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
/* ARGSUSED */
int
getlogin(p, uap, retval)
	struct proc *p;
	struct getlogin_args /* {
		syscallarg(char *) namebuf;
		syscallarg(u_int) namelen;
	} */ *uap;
	register_t *retval;
{

	if (SCARG(uap, namelen) > sizeof (p->p_pgrp->pg_session->s_login))
		SCARG(uap, namelen) = sizeof (p->p_pgrp->pg_session->s_login);
	return (copyout((caddr_t) p->p_pgrp->pg_session->s_login,
	    (caddr_t) SCARG(uap, namebuf), SCARG(uap, namelen)));
}

/*
 * Set login name.
 */
/* ARGSUSED */
int
setlogin(p, uap, retval)
	struct proc *p;
	struct setlogin_args /* {
		syscallarg(char *) namebuf;
	} */ *uap;
	register_t *retval;
{
	int error;

	if (error = suser(p->p_ucred, &p->p_acflag))
		return (error);
	error = copyinstr((caddr_t) SCARG(uap, namebuf),
	    (caddr_t) p->p_pgrp->pg_session->s_login,
	    sizeof (p->p_pgrp->pg_session->s_login) - 1, (u_int *)0);
	if (error == ENAMETOOLONG)
		error = EINVAL;
	return (error);
}

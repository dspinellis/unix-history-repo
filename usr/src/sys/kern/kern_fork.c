/*
 * Copyright (c) 1982, 1986, 1989, 1991 Regents of the University of California.
 * All rights reserved.
 *
 * %sccs.include.redist.c%
 *
 *	@(#)kern_fork.c	7.42 (Berkeley) %G%
 */

#include <sys/param.h>
#include <sys/systm.h>
#include <sys/map.h>
#include <sys/filedesc.h>
#include <sys/kernel.h>
#include <sys/malloc.h>
#include <sys/proc.h>
#include <sys/resourcevar.h>
#include <sys/vnode.h>
#include <sys/file.h>
#include <sys/acct.h>
#include <sys/ktrace.h>

struct fork_args {
	int	dummy;
};
/* ARGSUSED */
fork(p, uap, retval)
	struct proc *p;
	struct fork_args *uap;
	int retval[];
{

	return (fork1(p, 0, retval));
}

/* ARGSUSED */
vfork(p, uap, retval)
	struct proc *p;
	struct fork_args *uap;
	int retval[];
{

	return (fork1(p, 1, retval));
}

int	nprocs = 1;		/* process 0 */

fork1(p1, isvfork, retval)
	register struct proc *p1;
	int isvfork, retval[];
{
	register struct proc *p2;
	register uid_t uid;
	struct proc *newproc;
	struct proc **hash;
	int count;
	static int nextpid, pidchecked = 0;

	/*
	 * Although process entries are dynamically created, we still keep
	 * a global limit on the maximum number we will create.  Don't allow
	 * a nonprivileged user to bring the system within one of the global
	 * limit; don't let root exceed the limit. The variable nprocs is
	 * the current number of processes, maxproc is the limit.
	 */
	uid = p1->p_cred->p_ruid;
	if (nprocs >= maxproc || uid == 0 && nprocs >= maxproc + 1) {
		tablefull("proc");
		return (EAGAIN);
	}
	/*
	 * Increment the count of procs running with this uid. Don't allow
	 * a nonprivileged user to exceed their current limit.
	 */
	count = chgproccnt(uid, 1);
	if (uid != 0 && count > p1->p_rlimit[RLIMIT_NPROC].rlim_cur) {
		(void)chgproccnt(uid, -1);
		return (EAGAIN);
	}

	/* Allocate new proc. */
	MALLOC(newproc, struct proc *, sizeof(struct proc), M_PROC, M_WAITOK);

	/*
	 * Find an unused process ID.  We remember a range of unused IDs
	 * ready to use (from nextpid+1 through pidchecked-1).
	 */
	nextpid++;
retry:
	/*
	 * If the process ID prototype has wrapped around,
	 * restart somewhat above 0, as the low-numbered procs
	 * tend to include daemons that don't exit.
	 */
	if (nextpid >= PID_MAX) {
		nextpid = 100;
		pidchecked = 0;
	}
	if (nextpid >= pidchecked) {
		int doingzomb = 0;

		pidchecked = PID_MAX;
		/*
		 * Scan the active and zombie procs to check whether this pid
		 * is in use.  Remember the lowest pid that's greater
		 * than nextpid, so we can avoid checking for a while.
		 */
		p2 = (struct proc *)allproc;
again:
		for (; p2 != NULL; p2 = p2->p_nxt) {
			while (p2->p_pid == nextpid ||
			    p2->p_pgrp->pg_id == nextpid) {
				nextpid++;
				if (nextpid >= pidchecked)
					goto retry;
			}
			if (p2->p_pid > nextpid && pidchecked > p2->p_pid)
				pidchecked = p2->p_pid;
			if (p2->p_pgrp->pg_id > nextpid && 
			    pidchecked > p2->p_pgrp->pg_id)
				pidchecked = p2->p_pgrp->pg_id;
		}
		if (!doingzomb) {
			doingzomb = 1;
			p2 = zombproc;
			goto again;
		}
	}


	/*
	 * Link onto allproc (this should probably be delayed).
	 * Heavy use of volatile here to prevent the compiler from
	 * rearranging code.  Yes, it *is* terribly ugly, but at least
	 * it works.
	 */
	nprocs++;
	p2 = newproc;
#define	Vp2 ((volatile struct proc *)p2)
	Vp2->p_stat = SIDL;			/* protect against others */
	Vp2->p_pid = nextpid;
	/*
	 * This is really:
	 *	p2->p_nxt = allproc;
	 *	allproc->p_prev = &p2->p_nxt;
	 *	p2->p_prev = &allproc;
	 *	allproc = p2;
	 * The assignment via allproc is legal since it is never NULL.
	 */
	*(volatile struct proc **)&Vp2->p_nxt = allproc;
	*(volatile struct proc ***)&allproc->p_prev =
	    (volatile struct proc **)&Vp2->p_nxt;
	*(volatile struct proc ***)&Vp2->p_prev = &allproc;
	allproc = Vp2;
#undef Vp2
	p2->p_link = NULL;			/* shouldn't be necessary */
	p2->p_rlink = NULL;			/* shouldn't be necessary */

	/* Insert on the hash chain. */
	hash = &pidhash[PIDHASH(p2->p_pid)];
	p2->p_hash = *hash;
	*hash = p2;

	/*
	 * Make a proc table entry for the new process.
	 * Start by zeroing the section of proc that is zero-initialized,
	 * then copy the section that is copied directly from the parent.
	 */
	bzero(&p2->p_startzero,
	    (unsigned) ((caddr_t)&p2->p_endzero - (caddr_t)&p2->p_startzero));
	bcopy(&p1->p_startcopy, &p2->p_startcopy,
	    (unsigned) ((caddr_t)&p2->p_endcopy - (caddr_t)&p2->p_startcopy));

	/*
	 * Duplicate sub-structures as needed.
	 * Increase reference counts on shared objects.
	 * The p_stats and p_sigacts substructs are set in vm_fork.
	 */
	p2->p_flag = SLOAD;
	if (p1->p_flag & SPROFIL)
		startprofclock(p2);
	MALLOC(p2->p_cred, struct pcred *, sizeof(struct pcred),
	    M_SUBPROC, M_WAITOK);
	bcopy(p1->p_cred, p2->p_cred, sizeof(*p2->p_cred));
	p2->p_cred->p_refcnt = 1;
	crhold(p1->p_ucred);

	p2->p_fd = fdcopy(p1);
	/*
	 * If p_limit is still copy-on-write, bump refcnt,
	 * otherwise get a copy that won't be modified.
	 * (If PL_SHAREMOD is clear, the structure is shared
	 * copy-on-write.)
	 */
	if (p1->p_limit->p_lflags & PL_SHAREMOD)
		p2->p_limit = limcopy(p1->p_limit);
	else {
		p2->p_limit = p1->p_limit;
		p2->p_limit->p_refcnt++;
	}

	if (p1->p_session->s_ttyvp != NULL && p1->p_flag & SCTTY)
		p2->p_flag |= SCTTY;
	if (isvfork)
		p2->p_flag |= SPPWAIT;
	p2->p_pgrpnxt = p1->p_pgrpnxt;
	p1->p_pgrpnxt = p2;
	p2->p_pptr = p1;
	p2->p_osptr = p1->p_cptr;
	if (p1->p_cptr)
		p1->p_cptr->p_ysptr = p2;
	p1->p_cptr = p2;
#ifdef KTRACE
	/*
	 * Copy traceflag and tracefile if enabled.
	 * If not inherited, these were zeroed above.
	 */
	if (p1->p_traceflag&KTRFAC_INHERIT) {
		p2->p_traceflag = p1->p_traceflag;
		if ((p2->p_tracep = p1->p_tracep) != NULL)
			VREF(p2->p_tracep);
	}
#endif

	/*
	 * This begins the section where we must prevent the parent
	 * from being swapped.
	 */
	p1->p_flag |= SKEEP;
	/*
	 * Set return values for child before vm_fork,
	 * so they can be copied to child stack.
	 * We return parent pid, and mark as child in retval[1].
	 * NOTE: the kernel stack may be at a different location in the child
	 * process, and thus addresses of automatic variables (including retval)
	 * may be invalid after vm_fork returns in the child process.
	 */
	retval[0] = p1->p_pid;
	retval[1] = 1;
	if (vm_fork(p1, p2, isvfork)) {
		/*
		 * Child process.  Set start time and get to work.
		 */
		(void) splclock();
		p2->p_stats->p_start = time;
		(void) spl0();
		p2->p_acflag = AFORK;
		return (0);
	}

	/*
	 * Make child runnable and add to run queue.
	 */
	(void) splhigh();
	p2->p_stat = SRUN;
	setrq(p2);
	(void) spl0();

	/*
	 * Now can be swapped.
	 */
	p1->p_flag &= ~SKEEP;

	/*
	 * Preserve synchronization semantics of vfork.
	 * If waiting for child to exec or exit, set SPPWAIT
	 * on child, and sleep on our proc (in case of exit).
	 */
	if (isvfork)
		while (p2->p_flag & SPPWAIT)
			tsleep((caddr_t)p1, PWAIT, "ppwait", 0);

	/*
	 * Return child pid to parent process,
	 * marking us as parent via retval[1].
	 */
	retval[0] = p2->p_pid;
	retval[1] = 0;
	return (0);
}

/*
 * Copyright (c) 1982, 1986, 1989 Regents of the University of California.
 * All rights reserved.
 *
 * Redistribution is only permitted until one year after the first shipment
 * of 4.4BSD by the Regents.  Otherwise, redistribution and use in source and
 * binary forms are permitted provided that: (1) source distributions retain
 * this entire copyright notice and comment, and (2) distributions including
 * binaries display the following acknowledgement:  This product includes
 * software developed by the University of California, Berkeley and its
 * contributors'' in the documentation or other materials provided with the
 * distribution and in all advertising materials mentioning features or use
 * of this software.  Neither the name of the University nor the names of
 * its contributors may be used to endorse or promote products derived from
 * this software without specific prior written permission.
 * THIS SOFTWARE IS PROVIDED AS IS'' AND WITHOUT ANY EXPRESS OR IMPLIED
 * WARRANTIES, INCLUDING, WITHOUT LIMITATION, THE IMPLIED WARRANTIES OF
 * MERCHANTABILITY AND FITNESS FOR A PARTICULAR PURPOSE.
 *
 *	@(#)kern_fork.c	7.20 (Berkeley) 7/27/90
 */

#include "param.h"
#include "systm.h"
#include "map.h"
#include "user.h"
#include "kernel.h"
#include "proc.h"
#include "vnode.h"
#include "seg.h"
#include "vm.h"
#include "text.h"
#include "file.h"
#include "acct.h"
#include "ktrace.h"
#include "../ufs/quota.h"

#include "machine/reg.h"
#include "machine/pte.h"
#include "machine/psl.h"

/*
 * fork system call.
 */
/* ARGSUSED */
fork(p, uap, retval)
	struct proc *p;
	struct args *uap;
	int retval[];
{
	int error;

	u.u_cdmap = zdmap;
	u.u_csmap = zdmap;
	if (error = swpexpand(u.u_dsize, u.u_ssize, &u.u_cdmap, &u.u_csmap)) {
		retval[1] = 0;
		return (error);
	}
	return (fork1(p, 0, retval));
}

/* ARGSUSED */
vfork(p, uap, retval)
	struct proc *p;
	struct args *uap;
	int retval[];
{

	return (fork1(p, 1, retval));
}

fork1(p1, isvfork, retval)
	register struct proc *p1;
	int isvfork, retval[];
{
	register struct proc *p2;
	register int a;

	a = 0;
	if (p1->p_uid != 0) {
		for (p2 = allproc; p2; p2 = p2->p_nxt)
			if (p2->p_uid == p1->p_uid)
				a++;
		for (p2 = zombproc; p2; p2 = p2->p_nxt)
			if (p2->p_uid == p1->p_uid)
				a++;
	}
	/*
	 * Disallow if
	 *  No processes at all;
	 *  not su and too many procs owned; or
	 *  not su and would take last slot.
	 */
	p2 = freeproc;
	if (p2==NULL)
		tablefull("proc");
	if (p2 == NULL ||
	    (p1->p_uid != 0 && (p2->p_nxt == NULL || a > MAXUPRC))) {
		if (!isvfork) {
			(void) vsexpand((segsz_t)0, &u.u_cdmap, 1);
			(void) vsexpand((segsz_t)0, &u.u_csmap, 1);
		}
		retval[1] = 0;
		return (EAGAIN);
	}
	if (newproc(isvfork)) {
		retval[0] = p1->p_pid;
		retval[1] = 1;  /* child */
		u.u_acflag = AFORK;
		return (0);
	}
	retval[0] = p2->p_pid;
	retval[1] = 0;
	return (0);
}

/*
 * Create a new process-- the internal version of
 * sys fork.
 * It returns 1 in the new process, 0 in the old.
 */
newproc(isvfork)
	int isvfork;
{
	register struct proc *rpp, *rip;
	register int n;
	register struct file *fp;
	static int pidchecked = 0;

	/*
	 * First, just locate a slot for a process
	 * and copy the useful info from this process into it.
	 * The panic "cannot happen" because fork has already
	 * checked for the existence of a slot.
	 */
	mpid++;
retry:
	if (mpid >= PID_MAX) {
		mpid = 100;
		pidchecked = 0;
	}
	if (mpid >= pidchecked) {
		int doingzomb = 0;

		pidchecked = PID_MAX;
		/*
		 * Scan the proc table to check whether this pid
		 * is in use.  Remember the lowest pid that's greater
		 * than mpid, so we can avoid checking for a while.
		 */
		rpp = allproc;
again:
		for (; rpp != NULL; rpp = rpp->p_nxt) {
			if (rpp->p_pid == mpid || rpp->p_pgrp->pg_id == mpid) {
				mpid++;
				if (mpid >= pidchecked)
					goto retry;
			}
			if (rpp->p_pid > mpid && pidchecked > rpp->p_pid)
				pidchecked = rpp->p_pid;
			if (rpp->p_pgrp->pg_id > mpid && 
			    pidchecked > rpp->p_pgrp->pg_id)
				pidchecked = rpp->p_pgrp->pg_id;
		}
		if (!doingzomb) {
			doingzomb = 1;
			rpp = zombproc;
			goto again;
		}
	}
	if ((rpp = freeproc) == NULL)
		panic("no procs");

	freeproc = rpp->p_nxt;			/* off freeproc */
	rpp->p_nxt = allproc;			/* onto allproc */
	rpp->p_nxt->p_prev = &rpp->p_nxt;	/*   (allproc is never NULL) */
	rpp->p_prev = &allproc;
	allproc = rpp;

	/*
	 * Make a proc table entry for the new process.
	 */
	rip = u.u_procp;
#if defined(tahoe)
	rpp->p_ckey = rip->p_ckey;
	rpp->p_dkey = 0;
#endif
	rpp->p_stat = SIDL;
	timerclear(&rpp->p_realtimer.it_value);
	rpp->p_flag = SLOAD | (rip->p_flag & (SPAGV|SHPUX));
	if (rip->p_session->s_ttyvp != NULL && rip->p_flag & SCTTY)
		rpp->p_flag |= SCTTY;
	if (isvfork) {
		rpp->p_flag |= SVFORK;
		rpp->p_ndx = rip->p_ndx;
	} else
		rpp->p_ndx = rpp - proc;
	bcopy(rip->p_comm, rpp->p_comm, MAXCOMLEN+1);
	bcopy(rip->p_logname, rpp->p_logname, MAXLOGNAME);
	rpp->p_uid = rip->p_uid;
	rpp->p_ruid = rip->p_ruid;
	rpp->p_rgid = rip->p_rgid;
	rpp->p_pgrp = rip->p_pgrp;
	rpp->p_pgrpnxt = rip->p_pgrpnxt;
	rip->p_pgrpnxt = rpp;
	rpp->p_nice = rip->p_nice;
	rpp->p_textp = isvfork ? 0 : rip->p_textp;
	rpp->p_pid = mpid;
	rpp->p_ppid = rip->p_pid;
	rpp->p_pptr = rip;
	rpp->p_osptr = rip->p_cptr;
	if (rip->p_cptr)
		rip->p_cptr->p_ysptr = rpp;
	rpp->p_ysptr = NULL;
	rpp->p_cptr = NULL;
	rip->p_cptr = rpp;
	rpp->p_time = 0;
	bzero((caddr_t)&rpp->p_utime, sizeof (struct timeval));
	bzero((caddr_t)&rpp->p_stime, sizeof (struct timeval));
	rpp->p_cpu = 0;
	rpp->p_sigmask = rip->p_sigmask;
	rpp->p_sigcatch = rip->p_sigcatch;
	rpp->p_sigignore = rip->p_sigignore;
	/* take along any pending signals like stops? */
	if (isvfork) {
		rpp->p_tsize = rpp->p_dsize = rpp->p_mmsize = rpp->p_ssize = 0;
		rpp->p_szpt = clrnd(ctopt(HIGHPAGES));
		forkstat.cntvfork++;
		forkstat.sizvfork += rip->p_dsize + rip->p_ssize;
	} else {
		rpp->p_tsize = rip->p_tsize;
		rpp->p_dsize = rip->p_dsize;
		rpp->p_mmsize = rip->p_mmsize;
		rpp->p_ssize = rip->p_ssize;
		rpp->p_szpt = rip->p_szpt;
		forkstat.cntfork++;
		forkstat.sizfork += rip->p_dsize + rip->p_ssize;
	}
#ifdef KTRACE
	if (rip->p_traceflag&KTRFAC_INHERIT) {
		rpp->p_traceflag = rip->p_traceflag;
		if ((rpp->p_tracep = rip->p_tracep) != NULL)
			VREF(rpp->p_tracep);
	} else {
		rpp->p_tracep = NULL;
		rpp->p_traceflag = 0;
	}
#endif
	rpp->p_rssize = 0;
	rpp->p_maxrss = rip->p_maxrss;
	rpp->p_wchan = 0;
	rpp->p_slptime = 0;
	rpp->p_pctcpu = 0;
	rpp->p_cpticks = 0;
	{
	struct proc **hash = &pidhash[PIDHASH(rpp->p_pid)];

	rpp->p_hash = *hash;
	*hash = rpp;
	}
	multprog++;

	/*
	 * Increase reference counts on shared objects.
	 */
	for (n = 0; n <= u.u_lastfile; n++) {
		fp = u.u_ofile[n];
		if (fp == NULL)
			continue;
		fp->f_count++;
	}
	VREF(u.u_cdir);
	if (u.u_rdir)
		VREF(u.u_rdir);
	crhold(u.u_cred);

	/*
	 * This begins the section where we must prevent the parent
	 * from being swapped.
	 */
	rip->p_flag |= SKEEP;
	if (procdup(rpp, isvfork)) {
		(void) splclock();
		u.u_start = time;
		(void) spl0();
		return (1);
	}

	/*
	 * Make child runnable and add to run queue.
	 */
	(void) splclock();
	rpp->p_stat = SRUN;
	setrq(rpp);
	(void) spl0();

	/*
	 * Cause child to take a non-local goto as soon as it runs.
	 * On older systems this was done with SSWAP bit in proc
	 * table; on VAX we use u.u_pcb.pcb_sswap so don't need
	 * to do rpp->p_flag |= SSWAP.  Actually do nothing here.
	 */
	/* rpp->p_flag |= SSWAP; */

	/*
	 * Now can be swapped.
	 */
	rip->p_flag &= ~SKEEP;

	/*
	 * If vfork make chain from parent process to child
	 * (where virtal memory is temporarily).  Wait for
	 * child to finish, steal virtual memory back,
	 * and wakeup child to let it die.
	 */
	if (isvfork) {
		u.u_procp->p_xlink = rpp;
		u.u_procp->p_flag |= SNOVM;
		while (rpp->p_flag & SVFORK)
			sleep((caddr_t)rpp, PZERO - 1);
		if ((rpp->p_flag & SLOAD) == 0)
			panic("newproc vfork");
		uaccess(rpp, Vfmap, &vfutl);
		u.u_procp->p_xlink = 0;
		vpassvm(rpp, u.u_procp, &vfutl, &u, Vfmap);
		u.u_procp->p_flag &= ~SNOVM;
		rpp->p_ndx = rpp - proc;
		rpp->p_flag |= SVFDONE;
		wakeup((caddr_t)rpp);
	}

	/*
	 * 0 return means parent.
	 */
	return (0);
}

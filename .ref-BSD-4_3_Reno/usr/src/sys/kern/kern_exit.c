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
 *	@(#)kern_exit.c	7.26 (Berkeley) 6/28/90
 */

#include "param.h"
#include "systm.h"
#include "map.h"
#include "ioctl.h"
#include "tty.h"
#include "user.h"
#include "kernel.h"
#include "proc.h"
#include "buf.h"
#include "wait.h"
#include "vm.h"
#include "file.h"
#include "vnode.h"
#include "syslog.h"
#include "malloc.h"

#include "machine/reg.h"
#ifdef COMPAT_43
#include "machine/psl.h"
#endif

/*
 * Exit system call: pass back caller's arg
 */
/* ARGSUSED */
rexit(p, uap, retval)
	struct proc *p;
	struct args {
		int	rval;
	} *uap;
	int *retval;
{

	return (exit(p, W_EXITCODE(uap->rval, 0)));
}

/*
 * Release resources.
 * Save u. area for parent to look at.
 * Enter zombie state.
 * Wake up parent and init processes,
 * and dispose of children.
 */
exit(p, rv)
	struct proc *p;
	int rv;
{
	register int i;
	register struct proc *q, *nq;
	register struct proc **pp;

#ifdef PGINPROF
	vmsizmon();
#endif
	MALLOC(p->p_ru, struct rusage *, sizeof(struct rusage),
		M_ZOMBIE, M_WAITOK);
	p->p_flag &= ~(STRC|SULOCK);
	p->p_flag |= SWEXIT;
	p->p_sigignore = ~0;
	p->p_sig = 0;
	p->p_cpticks = 0;
	p->p_pctcpu = 0;
	for (i = 0; i < NSIG; i++)
		u.u_signal[i] = SIG_IGN;
	untimeout(realitexpire, (caddr_t)p);
	/*
	 * Release virtual memory.  If we resulted from
	 * a vfork(), instead give the resources back to
	 * the parent.
	 */
	if ((p->p_flag & SVFORK) == 0) {
#ifdef MAPMEM
		if (u.u_mmap)
			(void) mmexit(p);
#endif
		vrelvm();
	} else {
		p->p_flag &= ~SVFORK;
		wakeup((caddr_t)p);
		while ((p->p_flag & SVFDONE) == 0)
			sleep((caddr_t)p, PZERO - 1);
		p->p_flag &= ~SVFDONE;
	}
	for (i = 0; i <= u.u_lastfile; i++) {
		struct file *f;

		f = u.u_ofile[i];
		if (f) {
			u.u_ofile[i] = NULL;
			u.u_pofile[i] = 0;
			(void) closef(f);
		}
	}
	if (SESS_LEADER(p)) {
		register struct session *sp = p->p_session;

		if (sp->s_ttyvp) {
			/*
			 * Controlling process.
			 * Signal foreground pgrp and revoke access
			 * to controlling terminal.
			 */
			if (sp->s_ttyp->t_pgrp)
				pgsignal(sp->s_ttyp->t_pgrp, SIGHUP, 1);
			vgoneall(sp->s_ttyvp);
			vrele(sp->s_ttyvp);
			sp->s_ttyvp = NULL;
			/*
			 * s_ttyp is not zero'd; we use this to indicate
			 * that the session once had a controlling terminal.
			 * (for logging and informational purposes)
			 */
		}
		sp->s_leader = 0;
	}
	VOP_LOCK(u.u_cdir);
	vput(u.u_cdir);
	if (u.u_rdir) {
		VOP_LOCK(u.u_rdir);
		vput(u.u_rdir);
	}
	u.u_rlimit[RLIMIT_FSIZE].rlim_cur = RLIM_INFINITY;
	(void) acct(p);
	crfree(u.u_cred);
#ifdef KTRACE
	/* 
	 * release trace file
	 */
	if (p->p_tracep)
		vrele(p->p_tracep);
#endif
	/*
	 * Freeing the user structure and kernel stack
	 * for the current process: have to run a bit longer
	 * using the pages which are about to be freed...
	 * vrelu will block memory allocation by raising ipl.
	 */
	vrelu(p, 0);
	vrelpt(p);
	if (*p->p_prev = p->p_nxt)		/* off allproc queue */
		p->p_nxt->p_prev = p->p_prev;
	if (p->p_nxt = zombproc)		/* onto zombproc */
		p->p_nxt->p_prev = &p->p_nxt;
	p->p_prev = &zombproc;
	zombproc = p;
	multprog--;
	p->p_stat = SZOMB;
	noproc = 1;
	for (pp = &pidhash[PIDHASH(p->p_pid)]; *pp; pp = &(*pp)->p_hash)
		if (*pp == p) {
			*pp = p->p_hash;
			goto done;
		}
	panic("exit");
done:
	if (p->p_pid == 1) {
		if (p->p_dsize == 0) {
			printf("Can't exec init (errno %d)\n", WEXITSTATUS(rv));
			for (;;)
				;
		} else
			panic("init died");
	}
	p->p_xstat = rv;
	*p->p_ru = u.u_ru;
	i = splclock();
	p->p_ru->ru_stime = p->p_stime;
	p->p_ru->ru_utime = p->p_utime;
	splx(i);
	ruadd(p->p_ru, &u.u_cru);
	if (p->p_cptr)		/* only need this if any child is S_ZOMB */
		wakeup((caddr_t)&proc[1]);
	fixjobc(p, 0);
	for (q = p->p_cptr; q != NULL; q = nq) {
		nq = q->p_osptr;
		if (nq != NULL)
			nq->p_ysptr = NULL;
		if (proc[1].p_cptr)
			proc[1].p_cptr->p_ysptr = q;
		q->p_osptr = proc[1].p_cptr;
		q->p_ysptr = NULL;
		proc[1].p_cptr = q;

		q->p_pptr = &proc[1];
		q->p_ppid = 1;
		/*
		 * Traced processes are killed
		 * since their existence means someone is screwing up.
		 */
		if (q->p_flag&STRC) {
			q->p_flag &= ~STRC;
			psignal(q, SIGKILL);
		}
	}
	p->p_cptr = NULL;
	psignal(p->p_pptr, SIGCHLD);
	wakeup((caddr_t)p->p_pptr);
#if defined(tahoe)
	dkeyrelease(p->p_dkey), p->p_dkey = 0;
	ckeyrelease(p->p_ckey), p->p_ckey = 0;
	u.u_pcb.pcb_savacc.faddr = (float *)NULL;
#endif
	swtch();
}

#ifdef COMPAT_43
owait(p, uap, retval)
	struct proc *p;
	register struct args {
		int	pid;
		int	*status;
		int	options;
		struct	rusage *rusage;
		int	compat;
	} *uap;
	int *retval;
{

	if ((u.u_ar0[PS] & PSL_ALLCC) != PSL_ALLCC) {
		uap->options = 0;
		uap->rusage = 0;
	} else {
		uap->options = u.u_ar0[R0];
		uap->rusage = (struct rusage *)u.u_ar0[R1];
	}
	uap->pid = WAIT_ANY;
	uap->status = 0;
	uap->compat = 1;
	return (wait1(p, uap, retval));
}

wait4(p, uap, retval)
	struct proc *p;
	struct args {
		int	pid;
		int	*status;
		int	options;
		struct	rusage *rusage;
		int	compat;
	} *uap;
	int *retval;
{

	uap->compat = 0;
	return (wait1(p, uap, retval));
}
#else
#define	wait1	wait4
#endif

/*
 * Wait system call.
 * Search for a terminated (zombie) child,
 * finally lay it to rest, and collect its status.
 * Look also for stopped (traced) children,
 * and pass back status from them.
 */
wait1(q, uap, retval)
	register struct proc *q;
	register struct args {
		int	pid;
		int	*status;
		int	options;
		struct	rusage *rusage;
#ifdef COMPAT_43
		int compat;
#endif
	} *uap;
	int retval[];
{
	register int f;
	register struct proc *p;
	int status, error;

	if (uap->pid == 0)
		uap->pid = -q->p_pgid;
#ifdef notyet
	if (uap->options &~ (WUNTRACED|WNOHANG))
		return (EINVAL);
#endif
loop:
	f = 0;
	for (p = q->p_cptr; p; p = p->p_osptr) {
		if (uap->pid != WAIT_ANY &&
		    p->p_pid != uap->pid && p->p_pgid != -uap->pid)
			continue;
		f++;
		if (p->p_stat == SZOMB) {
			retval[0] = p->p_pid;
#ifdef COMPAT_43
			if (uap->compat)
				retval[1] = p->p_xstat;
			else
#endif
			if (uap->status) {
				status = p->p_xstat;	/* convert to int */
				if (error = copyout((caddr_t)&status,
				    (caddr_t)uap->status, sizeof(status)))
					return (error);
			}
			if (uap->rusage && (error = copyout((caddr_t)p->p_ru,
			    (caddr_t)uap->rusage, sizeof (struct rusage))))
				return (error);
			pgrm(p);			/* off pgrp */
			p->p_xstat = 0;
			ruadd(&u.u_cru, p->p_ru);
			FREE(p->p_ru, M_ZOMBIE);
			p->p_ru = 0;
			p->p_stat = NULL;
			p->p_pid = 0;
			p->p_ppid = 0;
			if (*p->p_prev = p->p_nxt)	/* off zombproc */
				p->p_nxt->p_prev = p->p_prev;
			p->p_nxt = freeproc;		/* onto freeproc */
			freeproc = p;
			if (q = p->p_ysptr)
				q->p_osptr = p->p_osptr;
			if (q = p->p_osptr)
				q->p_ysptr = p->p_ysptr;
			if ((q = p->p_pptr)->p_cptr == p)
				q->p_cptr = p->p_osptr;
			p->p_pptr = 0;
			p->p_ysptr = 0;
			p->p_osptr = 0;
			p->p_cptr = 0;
			p->p_sig = 0;
			p->p_sigcatch = 0;
			p->p_sigignore = 0;
			p->p_sigmask = 0;
			/*p->p_pgrp = 0;*/
			p->p_flag = 0;
			p->p_wchan = 0;
			return (0);
		}
		if (p->p_stat == SSTOP && (p->p_flag & SWTED) == 0 &&
		    (p->p_flag & STRC || uap->options & WUNTRACED)) {
			p->p_flag |= SWTED;
			retval[0] = p->p_pid;
#ifdef COMPAT_43
			if (uap->compat) {
				retval[1] = W_STOPCODE(p->p_xstat);
				error = 0;
			} else
#endif
			if (uap->status) {
				status = W_STOPCODE(p->p_xstat);
				error = copyout((caddr_t)&status,
				    (caddr_t)uap->status, sizeof(status));
			} else
				error = 0;
			return (error);
		}
	}
	if (f == 0)
		return (ECHILD);
	if (uap->options & WNOHANG) {
		retval[0] = 0;
		return (0);
	}
	if (error = tsleep((caddr_t)q, PWAIT | PCATCH, "wait", 0))
		return (error);
	goto loop;
}

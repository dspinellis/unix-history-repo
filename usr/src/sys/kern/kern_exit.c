/*
 * Copyright (c) 1982, 1986, 1988 Regents of the University of California.
 * All rights reserved.  The Berkeley software License Agreement
 * specifies the terms and conditions for redistribution.
 *
 *	@(#)kern_exit.c	7.6 (Berkeley) %G%
 */

#include "param.h"
#include "systm.h"
#include "map.h"
#include "dir.h"
#include "user.h"
#include "kernel.h"
#include "proc.h"
#include "buf.h"
#include "wait.h"
#include "vm.h"
#include "file.h"
#include "inode.h"
#include "tty.h"
#include "syslog.h"
#include "malloc.h"

#include "machine/reg.h"
#ifdef COMPAT_43
#include "machine/psl.h"
#endif

/*
 * Exit system call: pass back caller's arg
 */
rexit()
{
	register struct a {
		int	rval;
	} *uap;
	union wait status;

	uap = (struct a *)u.u_ap;
	status.w_status = 0;
	status.w_retcode = uap->rval;
	exit(status.w_status);
}

/*
 * Release resources.
 * Save u. area for parent to look at.
 * Enter zombie state.
 * Wake up parent and init processes,
 * and dispose of children.
 */
exit(rv)
	int rv;			/* should be union wait (XXX) */
{
	register int i;
	register struct proc *p, *q, *nq;
	register int x;

#ifdef PGINPROF
	vmsizmon();
#endif
	p = u.u_procp;
	MALLOC(p->p_ru, struct rusage *, sizeof(struct rusage),
		M_ZOMBIE, M_WAITOK);
	p->p_flag &= ~(STRC|SULOCK);
	p->p_flag |= SWEXIT;
	p->p_sigignore = ~0;
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
	if ((p->p_flag & SVFORK) == 0)
		vrelvm();
	else {
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
			closef(f);
		}
	}
	if (SESS_LEADER(p)) {
		p->p_session->s_leader = 0;
		/* TODO: vhangup(); */
		if (u.u_ttyp) {
			u.u_ttyp->t_session = 0;
			u.u_ttyp->t_pgid = 0;
		}
	}
	ilock(u.u_cdir);
	iput(u.u_cdir);
	if (u.u_rdir) {
		ilock(u.u_rdir);
		iput(u.u_rdir);
	}
	u.u_rlimit[RLIMIT_FSIZE].rlim_cur = RLIM_INFINITY;
	acct();
#ifdef QUOTA
	qclean();
#endif
#ifdef KTRACE
	/* 
	 * release trace file
	 */
	if (p->p_tracep)
		irele(p->p_tracep);
#endif
	/*
	/*
	 * Freeing the user structure and kernel stack
	 * for the current process: have to run a bit longer
	 * using the pages which are about to be freed...
	 * vrelu will block memory allocation by raising ipl.
	 */
	vrelu(u.u_procp, 0);
	vrelpt(u.u_procp);
	if (*p->p_prev = p->p_nxt)		/* off allproc queue */
		p->p_nxt->p_prev = p->p_prev;
	if (p->p_nxt = zombproc)		/* onto zombproc */
		p->p_nxt->p_prev = &p->p_nxt;
	p->p_prev = &zombproc;
	zombproc = p;
	multprog--;
	p->p_stat = SZOMB;
	noproc = 1;
	i = PIDHASH(p->p_pid);
	x = p - proc;
	if (pidhash[i] == x)
		pidhash[i] = p->p_idhash;
	else {
		for (i = pidhash[i]; i != 0; i = proc[i].p_idhash)
			if (proc[i].p_idhash == x) {
				proc[i].p_idhash = p->p_idhash;
				goto done;
			}
		panic("exit");
	}
	if (p->p_pid == 1) {
		if (p->p_dsize == 0) {
			printf("Can't exec init (errno %d)\n", rv >> 8);
			for (;;)
				;
		} else
			panic("init died");
	}
done:
	p->p_xstat = rv;
	*p->p_ru = u.u_ru;
	ruadd(p->p_ru, &u.u_cru);
	if (p->p_cptr)		/* only need this if any child is S_ZOMB */
		wakeup((caddr_t)&proc[1]);
	if (PGRP_JOBC(p))
		p->p_pgrp->pg_jobc--;
	for (q = p->p_cptr; q != NULL; q = nq) {
		if (PGRP_JOBC(q))
			q->p_pgrp->pg_jobc--;
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
		 * Stopped processes are sent a hangup and a continue.
		 * This is designed to be ``safe'' for setuid
		 * processes since they must be willing to tolerate
		 * hangups anyways.
		 */
		if (q->p_flag&STRC) {
			q->p_flag &= ~STRC;
			psignal(q, SIGKILL);
		} else if (q->p_stat == SSTOP) {
			psignal(q, SIGHUP);
			psignal(q, SIGCONT);
		}
		/*
		 * Protect this process from future
		 * tty signals, clear TSTP/TTIN/TTOU if pending.
		 */
		(void) spgrp(q);
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
owait()
{
	struct a {
		int	pid;
		union	wait *status;
		int	options;
		struct	rusage *rusage;
	} *uap = (struct a *)u.u_ap;

	if ((u.u_ar0[PS] & PSL_ALLCC) != PSL_ALLCC) {
		uap->options = WSIGRESTART;
		uap->rusage = 0;
	} else {
		uap->options = u.u_ar0[R0] | WSIGRESTART;
		uap->rusage = (struct rusage *)u.u_ar0[R1];
	}
	uap->pid = WAIT_ANY;
	uap->status = 0;
	wait1(1);
}

wait4()
{
	wait1(0);
}
#endif

/*
 * Wait system call.
 * Search for a terminated (zombie) child,
 * finally lay it to rest, and collect its status.
 * Look also for stopped (traced) children,
 * and pass back status from them.
 */
#ifdef COMPAT_43
wait1(compat)
	int compat;
#else
wait4()
#endif
{
	register struct a {
		int	pid;
		union	wait *status;
		int	options;
		struct	rusage *rusage;
	} *uap = (struct a *)u.u_ap;
	register f;
	register struct proc *p, *q;
	union wait status;

	f = 0;
	q = u.u_procp;
	if (uap->pid == 0)
		uap->pid = -q->p_pgid;
	if (uap->options &~ (WUNTRACED|WNOHANG|WSIGRESTART)) {
		u.u_error = EINVAL;
		return;
	}
loop:
	for (p = q->p_cptr; p; p = p->p_osptr) {
		if (uap->pid != WAIT_ANY &&
		    p->p_pid != uap->pid && p->p_pgid != -uap->pid)
			continue;
		f++;
		if (p->p_stat == SZOMB) {
			pgrm(p);			/* off pgrp */
			u.u_r.r_val1 = p->p_pid;
#ifdef COMPAT_43
			if (compat)
				u.u_r.r_val2 = p->p_xstat;
			else
#endif
			if (uap->status) {
				status.w_status = p->p_xstat;
				if (u.u_error = copyout((caddr_t)&status,
				    (caddr_t)uap->status, sizeof(status)))
					return;
			}
			p->p_xstat = 0;
			if (uap->rusage)
				u.u_error = copyout((caddr_t)p->p_ru,
				    (caddr_t)uap->rusage,
				    sizeof (struct rusage));
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
			p->p_cursig = 0;
			return;
		}
		if (p->p_stat == SSTOP && (p->p_flag & SWTED) == 0 &&
		    (p->p_flag & STRC || uap->options & WUNTRACED)) {
			p->p_flag |= SWTED;
			u.u_r.r_val1 = p->p_pid;
#ifdef COMPAT_43
			if (compat)
				u.u_r.r_val2 = (p->p_cursig<<8) | WSTOPPED;
			else
#endif
			if (uap->status) {
				status.w_status = 0;
				status.w_stopval = WSTOPPED;
				status.w_stopsig = p->p_cursig;
				u.u_error = copyout((caddr_t)&status,
				    (caddr_t)uap->status, sizeof(status));
			}
			return;
		}
	}
	if (f == 0) {
		u.u_error = ECHILD;
		return;
	}
	if (uap->options & WNOHANG) {
		u.u_r.r_val1 = 0;
		return;
	}
	if (uap->options & WSIGRESTART && setjmp(&u.u_qsave)) {
		p = u.u_procp;
		if ((u.u_sigintr & sigmask(p->p_cursig)) != 0) {
			u.u_error = EINTR;
			return;
		}
		u.u_eosys = RESTARTSYS;
		return;
	}
	sleep((caddr_t)u.u_procp, PWAIT);
	goto loop;
}

/*
 * Copyright (c) 1982, 1986, 1989 Regents of the University of California.
 * All rights reserved.
 *
 * %sccs.include.redist.c%
 *
 *	@(#)kern_sig.c	7.25 (Berkeley) %G%
 */

#include "param.h"
#include "systm.h"
#include "user.h"
#include "vnode.h"
#include "proc.h"
#include "timeb.h"
#include "times.h"
#include "buf.h"
#include "seg.h"
#include "acct.h"
#include "uio.h"
#include "file.h"
#include "kernel.h"
#include "wait.h"
#include "ktrace.h"

#include "machine/reg.h"
#include "machine/psl.h"
#include "machine/mtpr.h"
#include "../vm/vm_param.h"

#define	ttystopsigmask	(sigmask(SIGTSTP)|sigmask(SIGTTIN)|sigmask(SIGTTOU))
#define	stopsigmask	(sigmask(SIGSTOP)|ttystopsigmask)
#define defaultignmask	(sigmask(SIGCONT)|sigmask(SIGIO)|sigmask(SIGURG)| \
			sigmask(SIGCHLD)|sigmask(SIGWINCH)|sigmask(SIGINFO))

/*
 * Can process p send the signal signo to process q?
 */
#define CANSIGNAL(p, q, signo) \
	((p)->p_uid == 0 || \
	    (p)->p_ruid == (q)->p_ruid || (p)->p_uid == (q)->p_ruid || \
	    (p)->p_ruid == (q)->p_uid || (p)->p_uid == (q)->p_uid || \
	    ((signo) == SIGCONT && (q)->p_session == (p)->p_session))

/* ARGSUSED */
sigaction(p, uap, retval)
	struct proc *p;
	register struct args {
		int	signo;
		struct	sigaction *nsa;
		struct	sigaction *osa;
	} *uap;
	int *retval;
{
	struct sigaction vec;
	register struct sigaction *sa;
	register int sig;
	int bit, error;

	sig = uap->signo;
	if (sig <= 0 || sig >= NSIG || sig == SIGKILL || sig == SIGSTOP)
		return (EINVAL);
	sa = &vec;
	if (uap->osa) {
		sa->sa_handler = u.u_signal[sig];
		sa->sa_mask = u.u_sigmask[sig];
		bit = sigmask(sig);
		sa->sa_flags = 0;
		if ((u.u_sigonstack & bit) != 0)
			sa->sa_flags |= SA_ONSTACK;
		if ((u.u_sigintr & bit) == 0)
			sa->sa_flags |= SA_RESTART;
		if (p->p_flag & SNOCLDSTOP)
			sa->sa_flags |= SA_NOCLDSTOP;
		if (error = copyout((caddr_t)sa, (caddr_t)uap->osa,
		    sizeof (vec)))
			return (error);
	}
	if (uap->nsa) {
		if (error = copyin((caddr_t)uap->nsa, (caddr_t)sa,
		    sizeof (vec)))
			return (error);
		setsigvec(p, sig, sa);
	}
	return (0);
}

setsigvec(p, sig, sa)
	register struct proc *p;
	int sig;
	register struct sigaction *sa;
{
	register int bit;

	bit = sigmask(sig);
	/*
	 * Change setting atomically.
	 */
	(void) splhigh();
	u.u_signal[sig] = sa->sa_handler;
	u.u_sigmask[sig] = sa->sa_mask &~ sigcantmask;
	if ((sa->sa_flags & SA_RESTART) == 0)
		u.u_sigintr |= bit;
	else
		u.u_sigintr &= ~bit;
	if (sa->sa_flags & SA_ONSTACK)
		u.u_sigonstack |= bit;
	else
		u.u_sigonstack &= ~bit;
	if (sig == SIGCHLD) {
		if (sa->sa_flags & SA_NOCLDSTOP)
			p->p_flag |= SNOCLDSTOP;
		else
			p->p_flag &= ~SNOCLDSTOP;
	}
	/*
	 * Set bit in p_sigignore for signals that are set to SIG_IGN,
	 * and for signals set to SIG_DFL where the default is to ignore.
	 * However, don't put SIGCONT in p_sigignore,
	 * as we have to restart the process.
	 */
	if (sa->sa_handler == SIG_IGN ||
	   (bit & defaultignmask && sa->sa_handler == SIG_DFL)) {
		p->p_sig &= ~bit;		/* never to be seen again */
		if (sig != SIGCONT)
			p->p_sigignore |= bit;	/* easier in psignal */
		p->p_sigcatch &= ~bit;
	} else {
		p->p_sigignore &= ~bit;
		if (sa->sa_handler == SIG_DFL)
			p->p_sigcatch &= ~bit;
		else
			p->p_sigcatch |= bit;
	}
	(void) spl0();
}

/*
 * Initialize signal state for process 0;
 * set to ignore signals that are ignored by default.
 */
siginit(p)
	struct proc *p;
{

	p->p_sigignore = defaultignmask &~ sigmask(SIGCONT);
}

/*
 * Reset signals for an exec of the specified process.
 */
execsigs(p)
	register struct proc *p;
{
	register int nc, mask;

	/*
	 * Reset caught signals.  Held signals remain held
	 * through p_sigmask (unless they were caught,
	 * and are now ignored by default).
	 */
	while (p->p_sigcatch) {
		nc = ffs((long)p->p_sigcatch);
		mask = sigmask(nc);
		p->p_sigcatch &= ~mask;
		if (mask & defaultignmask) {
			if (nc != SIGCONT)
				p->p_sigignore |= mask;
			p->p_sig &= ~mask;
		}
		u.u_signal[nc] = SIG_DFL;
	}
	/*
	 * Reset stack state to the user stack.
	 * Clear set of signals caught on the signal stack.
	 */
	u.u_onstack = 0;
	u.u_sigsp = 0;
	u.u_sigonstack = 0;
}

/*
 * Manipulate signal mask.
 * Note that we receive new mask, not pointer,
 * and return old mask as return value;
 * the library stub does the rest.
 */
sigprocmask(p, uap, retval)
	register struct proc *p;
	struct args {
		int	how;
		sigset_t mask;
	} *uap;
	int *retval;
{
	int error = 0;

	*retval = p->p_sigmask;
	(void) splhigh();

	switch (uap->how) {
	case SIG_BLOCK:
		p->p_sigmask |= uap->mask &~ sigcantmask;
		break;

	case SIG_UNBLOCK:
		p->p_sigmask &= ~uap->mask;
		break;

	case SIG_SETMASK:
		p->p_sigmask = uap->mask &~ sigcantmask;
		break;
	
	default:
		error = EINVAL;
		break;
	}
	(void) spl0();
	return (error);
}

/* ARGSUSED */
sigpending(p, uap, retval)
	struct proc *p;
	void *uap;
	int *retval;
{

	*retval = p->p_sig;
	return (0);
}

#ifdef COMPAT_43
/*
 * Generalized interface signal handler, 4.3-compatible.
 */
/* ARGSUSED */
osigvec(p, uap, retval)
	struct proc *p;
	register struct args {
		int	signo;
		struct	sigvec *nsv;
		struct	sigvec *osv;
	} *uap;
	int *retval;
{
	struct sigvec vec;
	register struct sigvec *sv;
	register int sig;
	int bit, error;

	sig = uap->signo;
	if (sig <= 0 || sig >= NSIG || sig == SIGKILL || sig == SIGSTOP)
		return (EINVAL);
	sv = &vec;
	if (uap->osv) {
		*(sig_t *)&sv->sv_handler = u.u_signal[sig];
		sv->sv_mask = u.u_sigmask[sig];
		bit = sigmask(sig);
		sv->sv_flags = 0;
		if ((u.u_sigonstack & bit) != 0)
			sv->sv_flags |= SV_ONSTACK;
		if ((u.u_sigintr & bit) != 0)
			sv->sv_flags |= SV_INTERRUPT;
		if (p->p_flag & SNOCLDSTOP)
			sv->sv_flags |= SA_NOCLDSTOP;
		if (error = copyout((caddr_t)sv, (caddr_t)uap->osv,
		    sizeof (vec)))
			return (error);
	}
	if (uap->nsv) {
		if (error = copyin((caddr_t)uap->nsv, (caddr_t)sv,
		    sizeof (vec)))
			return (error);
		sv->sv_flags ^= SA_RESTART;	/* opposite of SV_INTERRUPT */
		setsigvec(p, sig, (struct sigaction *)sv);
	}
	return (0);
}

osigblock(p, uap, retval)
	register struct proc *p;
	struct args {
		int	mask;
	} *uap;
	int *retval;
{

	(void) splhigh();
	*retval = p->p_sigmask;
	p->p_sigmask |= uap->mask &~ sigcantmask;
	(void) spl0();
	return (0);
}

osigsetmask(p, uap, retval)
	struct proc *p;
	struct args {
		int	mask;
	} *uap;
	int *retval;
{

	(void) splhigh();
	*retval = p->p_sigmask;
	p->p_sigmask = uap->mask &~ sigcantmask;
	(void) spl0();
	return (0);
}
#endif

/*
 * Suspend process until signal, providing mask to be set
 * in the meantime.  Note nonstandard calling convention:
 * libc stub passes mask, not pointer, to save a copyin.
 */
/* ARGSUSED */
sigsuspend(p, uap, retval)
	register struct proc *p;
	struct args {
		sigset_t mask;
	} *uap;
	int *retval;
{

	/*
	 * When returning from sigpause, we want
	 * the old mask to be restored after the
	 * signal handler has finished.  Thus, we
	 * save it here and mark the proc structure
	 * to indicate this (should be in u.).
	 */
	u.u_oldmask = p->p_sigmask;
	p->p_flag |= SOMASK;
	p->p_sigmask = uap->mask &~ sigcantmask;
	(void) tsleep((caddr_t)&u, PPAUSE | PCATCH, "pause", 0);
	/* always return EINTR rather than ERESTART... */
	return (EINTR);
}

/* ARGSUSED */
sigstack(p, uap, retval)
	struct proc *p;
	register struct args {
		struct	sigstack *nss;
		struct	sigstack *oss;
	} *uap;
	int *retval;
{
	struct sigstack ss;
	int error = 0;

	if (uap->oss && (error = copyout((caddr_t)&u.u_sigstack,
	    (caddr_t)uap->oss, sizeof (struct sigstack))))
		return (error);
	if (uap->nss && (error = copyin((caddr_t)uap->nss, (caddr_t)&ss,
	    sizeof (ss))) == 0)
		u.u_sigstack = ss;
	return (error);
}

/* ARGSUSED */
kill(cp, uap, retval)
	register struct proc *cp;
	register struct args {
		int	pid;
		int	signo;
	} *uap;
	int *retval;
{
	register struct proc *p;

	if ((unsigned) uap->signo >= NSIG)
		return (EINVAL);
	if (uap->pid > 0) {
		/* kill single process */
		p = pfind(uap->pid);
		if (p == 0)
			return (ESRCH);
		if (!CANSIGNAL(cp, p, uap->signo))
			return (EPERM);
		if (uap->signo)
			psignal(p, uap->signo);
		return (0);
	}
	switch (uap->pid) {
	case -1:		/* broadcast signal */
		return (killpg1(cp, uap->signo, 0, 1));
	case 0:			/* signal own process group */
		return (killpg1(cp, uap->signo, 0, 0));
	default:		/* negative explicit process group */
		return (killpg1(cp, uap->signo, -uap->pid, 0));
	}
	/* NOTREACHED */
}

#ifdef COMPAT_43
/* ARGSUSED */
okillpg(p, uap, retval)
	struct proc *p;
	register struct args {
		int	pgid;
		int	signo;
	} *uap;
	int *retval;
{

	if ((unsigned) uap->signo >= NSIG)
		return (EINVAL);
	return (killpg1(p, uap->signo, uap->pgid, 0));
}
#endif

/*
 * Common code for kill process group/broadcast kill.
 * cp is calling process.
 */
killpg1(cp, signo, pgid, all)
	register struct proc *cp;
	int signo, pgid, all;
{
	register struct proc *p;
	struct pgrp *pgrp;
	int f = 0;
	
	if (all)	
		/* 
		 * broadcast 
		 */
		for (p = allproc; p != NULL; p = p->p_nxt) {
			if (p->p_ppid == 0 || p->p_flag&SSYS || 
			    p == u.u_procp || !CANSIGNAL(cp, p, signo))
				continue;
			f++;
			if (signo)
				psignal(p, signo);
		}
	else {
		if (pgid == 0)		
			/* 
			 * zero pgid means send to my process group.
			 */
			pgrp = u.u_procp->p_pgrp;
		else {
			pgrp = pgfind(pgid);
			if (pgrp == NULL)
				return (ESRCH);
		}
		for (p = pgrp->pg_mem; p != NULL; p = p->p_pgrpnxt) {
			if (p->p_ppid == 0 || p->p_flag&SSYS ||
			    !CANSIGNAL(cp, p, signo))
				continue;
			f++;
			if (signo)
				psignal(p, signo);
		}
	}
	return (f ? 0 : ESRCH);
}

/*
 * Send the specified signal to
 * all processes with 'pgid' as
 * process group.
 */
gsignal(pgid, sig)
{
	struct pgrp *pgrp;

	if (pgid && (pgrp = pgfind(pgid)))
		pgsignal(pgrp, sig, 0);
}

/*
 * Send sig to every member of a process group.
 * If checktty is 1, limit to members which have a controlling
 * terminal.
 */
pgsignal(pgrp, sig, checkctty)
	struct pgrp *pgrp;
{
	register struct proc *p;

	if (pgrp)
		for (p = pgrp->pg_mem; p != NULL; p = p->p_pgrpnxt)
			if (checkctty == 0 || p->p_flag&SCTTY)
				psignal(p, sig);
}

/*
 * Send a signal caused by a trap to the current process.
 * If it will be caught immediately, deliver it with correct code.
 * Otherwise, post it normally.
 */
trapsignal(sig, code)
	register int sig;
	unsigned code;
{
	register struct proc *p = u.u_procp;	/* XXX */
	int mask;

	mask = sigmask(sig);
	if ((p->p_flag & STRC) == 0 && (p->p_sigcatch & mask) != 0 &&
	    (p->p_sigmask & mask) == 0) {
		u.u_ru.ru_nsignals++;
#ifdef KTRACE
		if (KTRPOINT(p, KTR_PSIG))
			ktrpsig(p->p_tracep, sig, u.u_signal[sig], 
				p->p_sigmask, code);
#endif
		sendsig(u.u_signal[sig], sig, p->p_sigmask, code);
		p->p_sigmask |= u.u_sigmask[sig] | mask;
	} else {
		u.u_code = code;	/* XXX for core dump/debugger */
		psignal(p, sig);
	}
}

/*
 * Send the specified signal to the specified process.
 * Most signals do not do anything directly to a process;
 * they set a flag that asks the process to do something to itself.
 * Exceptions:
 *   o When a stop signal is sent to a sleeping process that takes the default
 *     action, the process is stopped without awakening it.
 *   o SIGCONT restarts stopped processes (or puts them back to sleep)
 *     regardless of the signal action (eg, blocked or ignored).
 * Other ignored signals are discarded immediately.
 */
psignal(p, sig)
	register struct proc *p;
	register int sig;
{
	register int s;
	register sig_t action;
	int mask;

	if ((unsigned)sig >= NSIG || sig == 0)
		panic("psignal sig");
	mask = sigmask(sig);

	/*
	 * If proc is traced, always give parent a chance.
	 */
	if (p->p_flag & STRC)
		action = SIG_DFL;
	else {
		/*
		 * If the signal is being ignored,
		 * then we forget about it immediately.
		 * (Note: we don't set SIGCONT in p_sigignore,
		 * and if it is set to SIG_IGN,
		 * action will be SIG_DFL here.)
		 */
		if (p->p_sigignore & mask)
			return;
		if (p->p_sigmask & mask)
			action = SIG_HOLD;
		else if (p->p_sigcatch & mask)
			action = SIG_CATCH;
		else
			action = SIG_DFL;
	}
	switch (sig) {

	case SIGTERM:
		if ((p->p_flag&STRC) || action != SIG_DFL)
			break;
		/* FALLTHROUGH */

	case SIGKILL:
		if (p->p_nice > NZERO)
			p->p_nice = NZERO;
		break;

	case SIGCONT:
		p->p_sig &= ~stopsigmask;
		break;

	case SIGTSTP:
	case SIGTTIN:
	case SIGTTOU:
		/*
		 * If sending a tty stop signal to a member of an orphaned
		 * process group, discard the signal here if the action
		 * is default; don't stop the process below if sleeping,
		 * and don't clear any pending SIGCONT.
		 */
		if (p->p_pgrp->pg_jobc == 0 && action == SIG_DFL)
		        return;
		/* FALLTHROUGH */

	case SIGSTOP:
		p->p_sig &= ~sigmask(SIGCONT);
		break;
	}
	p->p_sig |= mask;

	/*
	 * Defer further processing for signals which are held,
	 * except that stopped processes must be continued by SIGCONT.
	 */
	if (action == SIG_HOLD && (sig != SIGCONT || p->p_stat != SSTOP))
		return;
	s = splhigh();
	switch (p->p_stat) {

	case SSLEEP:
		/*
		 * If process is sleeping uninterruptibly
		 * we can't interrupt the sleep... the signal will
		 * be noticed when the process returns through
		 * trap() or syscall().
		 */
		if ((p->p_flag & SSINTR) == 0)
			goto out;
		/*
		 * Process is sleeping and traced... make it runnable
		 * so it can discover the signal in issig() and stop
		 * for the parent.
		 */
		if (p->p_flag&STRC)
			goto run;
		/*
		 * When a sleeping process receives a stop
		 * signal, process immediately if possible.
		 * All other (caught or default) signals
		 * cause the process to run.
		 */
		if (mask & stopsigmask) {
			if (action != SIG_DFL)
				goto runfast;
			/*
			 * If a child in vfork(), stopping could
			 * cause deadlock.
			 */
			if (p->p_flag&SVFORK)
				goto out;
			p->p_sig &= ~mask;
			p->p_xstat = sig;
			if ((p->p_pptr->p_flag & SNOCLDSTOP) == 0)
				psignal(p->p_pptr, SIGCHLD);
			stop(p);
			goto out;
		} else
			goto runfast;
		/*NOTREACHED*/

	case SSTOP:
		/*
		 * If traced process is already stopped,
		 * then no further action is necessary.
		 */
		if (p->p_flag&STRC)
			goto out;
		switch (sig) {

		case SIGKILL:
			/*
			 * Kill signal always sets processes running.
			 */
			goto runfast;

		case SIGCONT:
			/*
			 * If SIGCONT is default (or ignored), we continue
			 * the process but don't leave the signal in p_sig,
			 * as it has no further action.  If SIGCONT is held,
			 * continue the process and leave the signal in p_sig.
			 * If the process catches SIGCONT, let it handle
			 * the signal itself.  If it isn't waiting on
			 * an event, then it goes back to run state.
			 * Otherwise, process goes back to sleep state.
			 */
			if (action == SIG_DFL)
				p->p_sig &= ~mask;
			if (action == SIG_CATCH)
				goto runfast;
			if (p->p_wchan == 0)
				goto run;
			p->p_stat = SSLEEP;
			goto out;

		case SIGSTOP:
		case SIGTSTP:
		case SIGTTIN:
		case SIGTTOU:
			/*
			 * Already stopped, don't need to stop again.
			 * (If we did the shell could get confused.)
			 */
			p->p_sig &= ~mask;		/* take it away */
			goto out;

		default:
			/*
			 * If process is sleeping interruptibly, then
			 * simulate a wakeup so that when it is continued,
			 * it will be made runnable and can look at the signal.
			 * But don't setrun the process, leave it stopped.
			 */
			if (p->p_wchan && p->p_flag & SSINTR)
				unsleep(p);
			goto out;
		}
		/*NOTREACHED*/

	default:
		/*
		 * SRUN, SIDL, SZOMB do nothing with the signal,
		 * other than kicking ourselves if we are running.
		 * It will either never be noticed, or noticed very soon.
		 */
		if (p == u.u_procp && !noproc)
			aston();
		goto out;
	}
	/*NOTREACHED*/

runfast:
	/*
	 * Raise priority to at least PUSER.
	 */
	if (p->p_pri > PUSER)
		p->p_pri = PUSER;
run:
	setrun(p);
out:
	splx(s);
}

/*
 * If the current process has a signal to process (should be caught
 * or cause termination, should interrupt current syscall),
 * return the signal number.  Stop signals with default action
 * are processed immediately, then cleared; they aren't returned.
 * This is asked at least once each time a process enters the
 * system (though this can usually be done without actually
 * calling issig by checking the pending signal masks.)
 */
issig()
{
	register struct proc *p = u.u_procp;		/* XXX */
	register int sig, mask;

	for (;;) {
		mask = p->p_sig &~ p->p_sigmask;
		if (p->p_flag&SVFORK)
			mask &= ~stopsigmask;
		if (mask == 0)	 	/* no signal to send */
			return (0);
		sig = ffs((long)mask);
		mask = sigmask(sig);
		/*
		 * We should see pending but ignored signals
		 * only if STRC was on when they were posted.
		 */
		if (mask & p->p_sigignore && (p->p_flag&STRC) == 0) {
			p->p_sig &= ~mask;
			continue;
		}
		if (p->p_flag&STRC && (p->p_flag&SVFORK) == 0) {
			/*
			 * If traced, always stop, and stay
			 * stopped until released by the parent.
			 */
			p->p_xstat = sig;
			psignal(p->p_pptr, SIGCHLD);
			do {
				stop(p);
				swtch();
			} while (!procxmt(p) && p->p_flag&STRC);

			/*
			 * If the traced bit got turned off,
			 * go back up to the top to rescan signals.
			 * This ensures that p_sig* and u_signal are consistent.
			 */
			if ((p->p_flag&STRC) == 0)
				continue;

			/*
			 * If parent wants us to take the signal,
			 * then it will leave it in p->p_xstat;
			 * otherwise we just look for signals again.
			 */
			p->p_sig &= ~mask;	/* clear the old signal */
			sig = p->p_xstat;
			if (sig == 0)
				continue;

			/*
			 * Put the new signal into p_sig.
			 * If signal is being masked,
			 * look for other signals.
			 */
			mask = sigmask(sig);
			p->p_sig |= mask;
			if (p->p_sigmask & mask)
				continue;
		}

		/*
		 * Decide whether the signal should be returned.
		 * Return the signal's number, or fall through
		 * to clear it from the pending mask.
		 */
		switch ((int)u.u_signal[sig]) {

		case SIG_DFL:
			/*
			 * Don't take default actions on system processes.
			 */
			if (p->p_ppid == 0)
				break;		/* == ignore */
			/*
			 * If there is a pending stop signal to process
			 * with default action, stop here,
			 * then clear the signal.  However,
			 * if process is member of an orphaned
			 * process group, ignore tty stop signals.
			 */
			if (mask & stopsigmask) {
				if (p->p_flag&STRC ||
		    		    (p->p_pgrp->pg_jobc == 0 &&
				    mask & ttystopsigmask))
					break;	/* == ignore */
				p->p_xstat = sig;
				stop(p);
				if ((p->p_pptr->p_flag & SNOCLDSTOP) == 0)
					psignal(p->p_pptr, SIGCHLD);
				swtch();
				break;
			} else if (mask & defaultignmask) {
				/*
				 * Except for SIGCONT, shouldn't get here.
				 * Default action is to ignore; drop it.
				 */
				break;		/* == ignore */
			} else
				return (sig);
			/*NOTREACHED*/

		case SIG_IGN:
			/*
			 * Masking above should prevent us ever trying
			 * to take action on an ignored signal other
			 * than SIGCONT, unless process is traced.
			 */
			if (sig != SIGCONT && (p->p_flag&STRC) == 0)
				printf("issig\n");
			break;		/* == ignore */

		default:
			/*
			 * This signal has an action, let
			 * psig process it.
			 */
			return (sig);
		}
		p->p_sig &= ~mask;		/* take the signal! */
	}
	/* NOTREACHED */
}

/*
 * Put the argument process into the stopped
 * state and notify the parent via wakeup.
 * Signals are handled elsewhere.
 * The process must not be on the run queue.
 */
stop(p)
	register struct proc *p;
{

	p->p_stat = SSTOP;
	p->p_flag &= ~SWTED;
	wakeup((caddr_t)p->p_pptr);
}

/*
 * Perform the action specified by the current signal.
 * The usual sequence is:
 *	if (sig = CURSIG(p))
 *		psig(sig);
 */
psig(sig)
	register int sig;
{
	register struct proc *p = u.u_procp;
	int mask, returnmask;
	register sig_t action;

	do {
#ifdef DIAGNOSTIC
		if (sig == 0)
			panic("psig");
#endif
		mask = sigmask(sig);
		p->p_sig &= ~mask;
		action = u.u_signal[sig];
#ifdef KTRACE
		if (KTRPOINT(p, KTR_PSIG))
			ktrpsig(p->p_tracep, sig, action, p->p_flag & SOMASK ?
				u.u_oldmask : p->p_sigmask, 0);
#endif
		if (action != SIG_DFL) {
#ifdef DIAGNOSTIC
			if (action == SIG_IGN || (p->p_sigmask & mask))
				panic("psig action");
#endif
			/*
			 * Set the new mask value and also defer further
			 * occurences of this signal.
			 *
			 * Special case: user has done a sigpause.  Here the
			 * current mask is not of interest, but rather the
			 * mask from before the sigpause is what we want
			 * restored after the signal processing is completed.
			 */
			(void) splhigh();
			if (p->p_flag & SOMASK) {
				returnmask = u.u_oldmask;
				p->p_flag &= ~SOMASK;
			} else
				returnmask = p->p_sigmask;
			p->p_sigmask |= u.u_sigmask[sig] | mask;
			(void) spl0();
			u.u_ru.ru_nsignals++;
			sendsig(action, sig, returnmask, 0);
			continue;
		}
		u.u_acflag |= AXSIG;
		switch (sig) {

		case SIGILL:
		case SIGIOT:
		case SIGBUS:
		case SIGQUIT:
		case SIGTRAP:
		case SIGEMT:
		case SIGFPE:
		case SIGSEGV:
		case SIGSYS:
			u.u_sig = sig;
			if (core() == 0)
				sig |= WCOREFLAG;
		}
		exit(p, W_EXITCODE(0, sig));
		/* NOTREACHED */
	} while (sig = CURSIG(p));
}

/*
 * Create a core image on the file "core".
 * It writes UPAGES block of the
 * user.h area followed by the entire
 * data+stack segments.
 */
core()
{
	register struct vnode *vp;
	register struct proc *p = u.u_procp;
	register struct nameidata *ndp = &u.u_nd;
	struct vattr vattr;
	int error;

	if (p->p_svuid != p->p_ruid || p->p_svgid != p->p_rgid)
		return (EFAULT);
	if (ctob(UPAGES + u.u_dsize + u.u_ssize) >=
	    u.u_rlimit[RLIMIT_CORE].rlim_cur)
		return (EFAULT);
	ndp->ni_segflg = UIO_SYSSPACE;
	ndp->ni_dirp = "core";
	if (error = vn_open(ndp, FCREAT|FWRITE, 0644))
		return (error);
	vp = ndp->ni_vp;
	VOP_LOCK(vp);
	if (vp->v_type != VREG ||
	    VOP_GETATTR(vp, &vattr, u.u_cred) ||
	    vattr.va_nlink != 1) {
		vput(vp);
		return (EFAULT);
	}
	VATTR_NULL(&vattr);
	vattr.va_size = 0;
	VOP_SETATTR(vp, &vattr, u.u_cred);
	u.u_acflag |= ACORE;
#ifdef HPUXCOMPAT
	/*
	 * BLETCH!  If we loaded from an HPUX format binary file
	 * we have to dump an HPUX style user struct so that the
	 * HPUX debuggers can grok it.
	 */
	if (u.u_pcb.pcb_flags & PCB_HPUXBIN)
		error = hpuxdumpu(vp, ndp->ni_cred);
	else
#endif
	error = vn_rdwr(UIO_WRITE, vp, (caddr_t)&u, ctob(UPAGES), (off_t)0,
	    UIO_SYSSPACE, IO_NODELOCKED|IO_UNIT, ndp->ni_cred, (int *)0);
	if (error == 0)
		error = vn_rdwr(UIO_WRITE, vp, u.u_daddr,
		    (int)ctob(u.u_dsize), (off_t)ctob(UPAGES), UIO_USERSPACE,
		    IO_NODELOCKED|IO_UNIT, ndp->ni_cred, (int *)0);
	if (error == 0)
		error = vn_rdwr(UIO_WRITE, vp,
		    trunc_page(USRSTACK - ctob(u.u_ssize)),
		    round_page(ctob(u.u_ssize)),
		    (off_t)ctob(UPAGES) + ctob(u.u_dsize), UIO_USERSPACE,
		    IO_NODELOCKED|IO_UNIT, ndp->ni_cred, (int *)0);
	vput(vp);
	return (error);
}

/*
 * Nonexistent system call-- signal process (may want to handle it).
 * Flag error in case process won't see signal immediately (blocked or ignored).
 */
/* ARGSUSED */
nosys(p, args, retval)
	struct proc *p;
	void *args;
	int *retval;
{

	psignal(p, SIGSYS);
	return (EINVAL);
}

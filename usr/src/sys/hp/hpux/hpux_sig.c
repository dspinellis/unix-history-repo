/*
 * Copyright (c) 1988 University of Utah.
 * Copyright (c) 1990 The Regents of the University of California.
 * All rights reserved.
 *
 * This code is derived from software contributed to Berkeley by
 * the Systems Programming Group of the University of Utah Computer
 * Science Department.
 *
 * %sccs.include.redist.c%
 *
 * from: Utah $Hdr: hpux_sig.c 1.4 92/01/20$
 *
 *	@(#)hpux_sig.c	7.12 (Berkeley) %G%
 */

/*
 * Signal related HPUX compatibility routines
 */

#ifdef HPUXCOMPAT

#include "param.h"
#include "systm.h"
#include "kernel.h"
#include "proc.h"
#include "signalvar.h"
#include "hpux.h"

/* indexed by HPUX signal number - 1 */
char hpuxtobsdsigmap[NSIG] = {
/*01*/	SIGHUP,  SIGINT, SIGQUIT, SIGILL,   SIGTRAP, SIGIOT,  SIGEMT,   SIGFPE,
/*09*/  SIGKILL, SIGBUS, SIGSEGV, SIGSYS,   SIGPIPE, SIGALRM, SIGTERM,  SIGUSR1,
/*17*/  SIGUSR2, SIGCHLD, 0,      SIGVTALRM,SIGPROF, SIGIO,   SIGWINCH, SIGSTOP,
/*25*/	SIGTSTP, SIGCONT,SIGTTIN, SIGTTOU,  SIGURG,  0,       0,        0
};

/* indexed by BSD signal number - 1 */
char bsdtohpuxsigmap[NSIG] = {
/*01*/	 1,  2,  3,  4,  5,  6,  7,  8,
/*09*/   9, 10, 11, 12, 13, 14, 15, 29,
/*17*/  24, 25, 26, 18, 27, 28, 22,  0,
/*25*/	 0, 20, 21, 23,  0, 16, 17,  0
};

/*
 * XXX: In addition to mapping the signal number we also have
 * to see if the "old" style signal mechinism is needed.
 * If so, we set the OUSIG flag.  This is not really correct
 * as under HP-UX "old" style handling can be set on a per
 * signal basis and we are setting it for all signals in one
 * swell foop.  I suspect we can get away with this since I
 * doubt any program of interest mixes the two semantics.
 */
struct hpuxsigvec_args {
	int	signo;
	struct	sigvec *nsv;
	struct	sigvec *osv;
};
hpuxsigvec(p, uap, retval)
	struct proc *p;
	register struct hpuxsigvec_args *uap;
	int *retval;
{
	struct sigvec vec;
	register struct sigacts *ps = p->p_sigacts;
	register struct sigvec *sv;
	register int sig;
	int bit, error;

	sig = hpuxtobsdsig(uap->signo);
	if (sig <= 0 || sig >= NSIG || sig == SIGKILL || sig == SIGSTOP)
		return (EINVAL);
	sv = &vec;
	if (uap->osv) {
		sv->sv_handler = ps->ps_sigact[sig];
		sv->sv_mask = ps->ps_catchmask[sig];
		bit = sigmask(sig);
		sv->sv_flags = 0;
		if ((ps->ps_sigonstack & bit) != 0)
			sv->sv_flags |= SV_ONSTACK;
		if ((ps->ps_sigintr & bit) != 0)
			sv->sv_flags |= SV_INTERRUPT;
#if 0
/* XXX -- SOUSIG no longer exists, do something here */
		if (p->p_flag & SOUSIG)
			sv->sv_flags |= HPUXSV_RESET;		/* XXX */
#endif
		error = copyout((caddr_t)sv, (caddr_t)uap->osv, sizeof (vec));
		if (error)
			return (error);
	}
	if (uap->nsv) {
		error = copyin((caddr_t)uap->nsv, (caddr_t)sv, sizeof (vec));
		if (error)
			return (error);
		if (sig == SIGCONT && sv->sv_handler == SIG_IGN)
			return (EINVAL);
		sv->sv_flags ^= SA_RESTART;
		setsigvec(p, sig, (struct sigaction *)sv);
#if 0
/* XXX -- SOUSIG no longer exists, do something here */
		if (sv->sv_flags & HPUXSV_RESET)
			p->p_flag |= SOUSIG;		/* XXX */
#endif
	}
	return (0);
}

struct hpuxsigblock_args {
	int	mask;
};
hpuxsigblock(p, uap, retval)
	register struct proc *p;
	struct hpuxsigblock_args *uap;
	int *retval;
{

	(void) splhigh();
	*retval = bsdtohpuxmask(p->p_sigmask);
	p->p_sigmask |= hpuxtobsdmask(uap->mask) &~ sigcantmask;
	(void) spl0();
	return (0);
}

struct hpuxsigsetmask_args {
	int	mask;
};
hpuxsigsetmask(p, uap, retval)
	struct proc *p;
	struct hpuxsigsetmask_args *uap;
	int *retval;
{

	(void) splhigh();
	*retval = bsdtohpuxmask(p->p_sigmask);
	p->p_sigmask = hpuxtobsdmask(uap->mask) &~ sigcantmask;
	(void) spl0();
	return (0);
}

struct hpuxsigpause_args {
	int	mask;
};
hpuxsigpause(p, uap, retval)
	struct proc *p;
	struct hpuxsigpause_args *uap;
	int *retval;
{

	uap->mask = hpuxtobsdmask(uap->mask);
	return (sigsuspend(p, uap, retval));
}

/* not totally correct, but close enuf' */
struct hpuxkill_args {
	int	pid;
	int	signo;
};
hpuxkill(p, uap, retval)
	struct proc *p;
	struct hpuxkill_args *uap;
	int *retval;
{

	if (uap->signo) {
		uap->signo = hpuxtobsdsig(uap->signo);
		if (uap->signo == 0)
			uap->signo = NSIG;
	}
	return (kill(p, uap, retval));
}

/*
 * The following (sigprocmask, sigpending, sigsuspend, sigaction are
 * POSIX calls.  Under BSD, the library routine dereferences the sigset_t
 * pointers before traping.  Not so under HP-UX.
 */

/*
 * Manipulate signal mask.
 * Note that we receive new mask, not pointer,
 * and return old mask as return value;
 * the library stub does the rest.
 */
struct hpuxsigprocmask_args {
	int		how;
	hpuxsigset_t	*set;
	hpuxsigset_t	*oset;
};
hpuxsigprocmask(p, uap, retval)
	register struct proc *p;
	struct hpuxsigprocmask_args *uap;
	int *retval;
{
	int mask, error = 0;
	hpuxsigset_t sigset;

	/*
	 * Copy out old mask first to ensure no errors.
	 * (proc sigmask should not be changed if call fails for any reason)
	 */
	if (uap->oset) {
		bzero((caddr_t)&sigset, sizeof(sigset));
		sigset.sigset[0] = bsdtohpuxmask(p->p_sigmask);
		if (copyout((caddr_t)&sigset, (caddr_t)uap->oset, sizeof(sigset)))
			return (EFAULT);
	}
	if (uap->set) {
		if (copyin((caddr_t)uap->set, (caddr_t)&sigset, sizeof(sigset)))
			return (EFAULT);
		mask = hpuxtobsdmask(sigset.sigset[0]);
		(void) splhigh();
		switch (uap->how) {
		case HPUXSIG_BLOCK:
			p->p_sigmask |= mask &~ sigcantmask;
			break;
		case HPUXSIG_UNBLOCK:
			p->p_sigmask &= ~mask;
			break;
		case HPUXSIG_SETMASK:
			p->p_sigmask = mask &~ sigcantmask;
			break;
		default:
			error = EINVAL;
			break;
		}
		(void) spl0();
	}
	return (error);
}

struct hpuxsigpending_args {
	hpuxsigset_t	*set;
};
hpuxsigpending(p, uap, retval)
	register struct proc *p;
	struct hpuxsigpending_args *uap;
	int *retval;
{
	hpuxsigset_t sigset;

	sigset.sigset[0] = bsdtohpuxmask(p->p_sig);
	return (copyout((caddr_t)&sigset, (caddr_t)uap->set, sizeof(sigset)));
}

struct hpuxsigsuspend_args {
	hpuxsigset_t	*set;
};
hpuxsigsuspend(p, uap, retval)
	register struct proc *p;
	struct hpuxsigsuspend_args *uap;
	int *retval;
{
	register struct sigacts *ps = p->p_sigacts;
	hpuxsigset_t sigset;
	int mask;

	if (copyin((caddr_t)uap->set, (caddr_t)&sigset, sizeof(sigset)))
		return (EFAULT);
	mask = hpuxtobsdmask(sigset.sigset[0]);
	ps->ps_oldmask = p->p_sigmask;
	ps->ps_flags |= SAS_OLDMASK;
	p->p_sigmask = mask &~ sigcantmask;
	(void) tsleep((caddr_t)ps, PPAUSE | PCATCH, "pause", 0);
	/* always return EINTR rather than ERESTART... */
	return (EINTR);
}

struct hpuxsigaction_args {
	int	signo;
	struct	hpuxsigaction *nsa;
	struct	hpuxsigaction *osa;
};
hpuxsigaction(p, uap, retval)
	struct proc *p;
	register struct hpuxsigaction_args *uap;
	int *retval;
{
	struct hpuxsigaction action;
	register struct sigacts *ps = p->p_sigacts;
	register struct hpuxsigaction *sa;
	register int sig;
	int bit;

	sig = hpuxtobsdsig(uap->signo);
	if (sig <= 0 || sig >= NSIG || sig == SIGKILL || sig == SIGSTOP)
		return (EINVAL);

	sa = &action;
	if (uap->osa) {
		sa->sa_handler = ps->ps_sigact[sig];
		bzero((caddr_t)&sa->sa_mask, sizeof(sa->sa_mask));
		sa->sa_mask.sigset[0] = bsdtohpuxmask(ps->ps_catchmask[sig]);
		bit = sigmask(sig);
		sa->sa_flags = 0;
		if ((ps->ps_sigonstack & bit) != 0)
			sa->sa_flags |= HPUXSA_ONSTACK;
#if 0
/* XXX -- SOUSIG no longer exists, do something here */
		if (p->p_flag & SOUSIG)
			sa->sa_flags |= HPUXSA_RESETHAND;	/* XXX */
#endif
		if (p->p_flag & SNOCLDSTOP)
			sa->sa_flags |= HPUXSA_NOCLDSTOP;
		if (copyout((caddr_t)sa, (caddr_t)uap->osa, sizeof (action)))
			return (EFAULT);
	}
	if (uap->nsa) {
		struct sigaction act;

		if (copyin((caddr_t)uap->nsa, (caddr_t)sa, sizeof (action)))
			return (EFAULT);
		if (sig == SIGCONT && sa->sa_handler == SIG_IGN)
			return (EINVAL);
		/*
		 * Create a sigaction struct for setsigvec
		 */
		act.sa_handler = sa->sa_handler;
		act.sa_mask = hpuxtobsdmask(sa->sa_mask.sigset[0]);
		act.sa_flags == SA_RESTART;
		if (sa->sa_flags & HPUXSA_ONSTACK)
			act.sa_flags |= SA_ONSTACK;
		if (sa->sa_flags & HPUXSA_NOCLDSTOP)
			act.sa_flags |= SA_NOCLDSTOP;
		setsigvec(p, sig, &act);
#if 0
/* XXX -- SOUSIG no longer exists, do something here */
		if (sa->sa_flags & HPUXSA_RESETHAND)
			p->p_flag |= SOUSIG;		/* XXX */
#endif
	}
	return (0);
}

#ifdef COMPAT_OHPUX
struct ohpuxssig_args {
	int	signo;
	sig_t	fun;
};
ohpuxssig(p, uap, retval)
	struct proc *p;
	struct ohpuxssig_args *uap;
	int *retval;
{
	register int a;
	struct sigaction vec;
	register struct sigaction *sa = &vec;

	a = hpuxtobsdsig(uap->signo);
	sa->sa_handler = uap->fun;
	/*
	 * Kill processes trying to use job control facilities
	 * (this'll help us find any vestiges of the old stuff).
	 */
	if ((a &~ 0377) ||
	    (sa->sa_handler != SIG_DFL && sa->sa_handler != SIG_IGN &&
	     ((int)sa->sa_handler) & 1)) {
		psignal(p, SIGSYS);
		return (0);
	}
	if (a <= 0 || a >= NSIG || a == SIGKILL || a == SIGSTOP ||
	    a == SIGCONT && sa->sa_handler == SIG_IGN)
		return (EINVAL);
	sa->sa_mask = 0;
	sa->sa_flags = 0;
	*retval = (int)p->p_sigacts->ps_sigact[a];
	setsigvec(p, a, sa);
#if 0
	p->p_flag |= SOUSIG;		/* mark as simulating old stuff */
#endif
	return (0);
}
#endif

/* signal numbers: convert from HPUX to BSD */
hpuxtobsdsig(sig)
	register int sig;
{
	if (--sig < 0 || sig >= NSIG)
		return(0);
	return((int)hpuxtobsdsigmap[sig]);
}

/* signal numbers: convert from BSD to HPUX */
bsdtohpuxsig(sig)
	register int sig;
{
	if (--sig < 0 || sig >= NSIG)
		return(0);
	return((int)bsdtohpuxsigmap[sig]);
}

/* signal masks: convert from HPUX to BSD (not pretty or fast) */
hpuxtobsdmask(mask)
	register int mask;
{
	register int nmask, sig, nsig;

	if (mask == 0 || mask == -1)
		return(mask);
	nmask = 0;
	for (sig = 1; sig < NSIG; sig++)
		if ((mask & sigmask(sig)) && (nsig = hpuxtobsdsig(sig)))
			nmask |= sigmask(nsig);
	return(nmask);
}

bsdtohpuxmask(mask)
	register int mask;
{
	register int nmask, sig, nsig;

	if (mask == 0 || mask == -1)
		return(mask);
	nmask = 0;
	for (sig = 1; sig < NSIG; sig++)
		if ((mask & sigmask(sig)) && (nsig = bsdtohpuxsig(sig)))
			nmask |= sigmask(nsig);
	return(nmask);
}
#endif

/*
 * Copyright (c) 1988 University of Utah.
 * Copyright (c) 1990 The Regents of the University of California.
 * All rights reserved.
 *
 * This code is derived from software contributed to Berkeley by
 * the Systems Programming Group of the University of Utah Computer
 * Science Department.
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
 * from: Utah $Hdr: hpux_compat.c 1.33 89/08/23$
 *
 *	@(#)hpux_sig.c	7.4 (Berkeley) 6/28/90
 */

/*
 * Signal related HPUX compatibility routines
 */

#ifdef HPUXCOMPAT

#include "param.h"
#include "systm.h"
#include "user.h"
#include "kernel.h"
#include "proc.h"
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
hpuxsigvec(p, uap, retval)
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

	sig = hpuxtobsdsig(uap->signo);
	if (sig <= 0 || sig >= NSIG || sig == SIGKILL || sig == SIGSTOP)
		return (EINVAL);
	sv = &vec;
	if (uap->osv) {
		sv->sv_handler = u.u_signal[sig];
		sv->sv_mask = u.u_sigmask[sig];
		bit = sigmask(sig);
		sv->sv_flags = 0;
		if ((u.u_sigonstack & bit) != 0)
			sv->sv_flags |= SV_ONSTACK;
		if ((u.u_sigintr & bit) != 0)
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
		setsigvec(p, sig, (struct sigaction *)sv);
#if 0
/* XXX -- SOUSIG no longer exists, do something here */
		if (sv->sv_flags & HPUXSV_RESET)
			p->p_flag |= SOUSIG;		/* XXX */
#endif
	}
	return (0);
}

hpuxsigblock(p, uap, retval)
	register struct proc *p;
	struct args {
		int	mask;
	} *uap;
	int *retval;
{

	(void) splhigh();
	*retval = bsdtohpuxmask(p->p_sigmask);
	p->p_sigmask |= hpuxtobsdmask(uap->mask) &~ sigcantmask;
	(void) spl0();
	return (0);
}

hpuxsigsetmask(p, uap, retval)
	struct proc *p;
	struct args {
		int	mask;
	} *uap;
	int *retval;
{

	(void) splhigh();
	*retval = bsdtohpuxmask(p->p_sigmask);
	p->p_sigmask = hpuxtobsdmask(uap->mask) &~ sigcantmask;
	(void) spl0();
	return (0);
}

hpuxsigpause(p, uap, retval)
	struct proc *p;
	struct args {
		int	mask;
	} *uap;
	int *retval;
{

	uap->mask = hpuxtobsdmask(uap->mask);
	return (sigsuspend(p, uap, retval));
}

/* not totally correct, but close enuf' */
hpuxkill(p, uap, retval)
	struct proc *p;
	struct args {
		int	pid;
		int	signo;
	} *uap;
	int *retval;
{

	if (uap->signo) {
		uap->signo = hpuxtobsdsig(uap->signo);
		if (uap->signo == 0)
			uap->signo = NSIG;
	}
	return (kill(p, uap, retval));
}

ohpuxssig(p, uap, retval)
	struct proc *p;
	struct args {
		int	signo;
		sig_t	fun;
	} *uap;
	int *retval;
{
	register int a;
	struct sigvec vec;
	register struct sigvec *sv = &vec;

	a = hpuxtobsdsig(uap->signo);
	sv->sv_handler = uap->fun;
	/*
	 * Kill processes trying to use job control facilities
	 * (this'll help us find any vestiges of the old stuff).
	 */
	if ((a &~ 0377) ||
	    (sv->sv_handler != SIG_DFL && sv->sv_handler != SIG_IGN &&
	     ((int)sv->sv_handler) & 1)) {
		psignal(p, SIGSYS);
		return (0);
	}
	if (a <= 0 || a >= NSIG || a == SIGKILL || a == SIGSTOP ||
	    a == SIGCONT && sv->sv_handler == SIG_IGN)
		return (EINVAL);
	sv->sv_mask = 0;
	sv->sv_flags = SV_INTERRUPT;
	*retval = (int)u.u_signal[a];
	setsigvec(p, a, (struct sigaction *)sv);
#if 0
	p->p_flag |= SOUSIG;		/* mark as simulating old stuff */
#endif
	return (0);
}

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

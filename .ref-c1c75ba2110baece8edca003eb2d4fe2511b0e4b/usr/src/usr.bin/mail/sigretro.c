/*
 * Copyright (c) 1980 Regents of the University of California.
 * All rights reserved.
 *
 * Redistribution and use in source and binary forms are permitted
 * provided that this notice is preserved and that due credit is given
 * to the University of California at Berkeley. The name of the University
 * may not be used to endorse or promote products derived from this
 * software without specific prior written permission. This software
 * is provided ``as is'' without express or implied warranty.
 */

#ifdef notdef
static char sccsid[] = "@(#)sigretro.c	5.3 (Berkeley) %G%";
#endif /* notdef */

#include <signal.h>
#include <errno.h>
#include <setjmp.h>
#include "sigretro.h"

/*
 * Retrofit new signal interface to old signal primitives.
 * Supported routines:
 *	sigsys(sig, func)
 *	sigset(sig, func)
 *	sighold(sig)
 *	sigrelse(sig)
 *	sigignore(sig)
 *	sigpause(sig)
 * Also,
 *	sigchild()
 *		to set all held signals to ignored signals in the
 *		child process after fork(2)
 */

typedef	int	(*sigtype)();

sigtype	sigdisp(), sighold(), sigignore();

/*
 * The following helps us keep the extended signal semantics together.
 * We remember for each signal the address of the function we're
 * supposed to call.  s_func is SIG_DFL / SIG_IGN if appropriate.
 */
struct sigtable {
	sigtype	s_func;			/* What to call */
	int	s_flag;			/* Signal flags; see below */
} sigtable[NSIG + 1];

/*
 * Signal flag values.
 */
#define	SHELD		1		/* Signal is being held */
#define	SDEFER		2		/* Signal occured while held */
#define	SSET		4		/* s_func is believable */
#define	SPAUSE		8		/* are pausing, waiting for sig */

jmp_buf	_pause;				/* For doing sigpause() */

/*
 * Approximate sigsys() system call
 * This is almost useless since one only calls sigsys()
 * in the child of a vfork().  If you have vfork(), you have new signals
 * anyway.  The real sigsys() does all the stuff needed to support
 * the real sigset() library.  We don't bother here, assuming that
 * you are either ignoring or defaulting a signal in the child.
 */
sigtype
sigsys(sig, func)
	sigtype func;
{
	sigtype old;

	old = sigdisp(sig);
	signal(sig, func);
	return(old);
}

/*
 * Set the (permanent) disposition of a signal.
 * If the signal is subsequently (or even now) held,
 * the action you set here can be enabled using sigrelse().
 */
sigtype
sigset(sig, func)
	sigtype func;
{
	sigtype old;
	int _sigtramp();
	extern int errno;

	if (sig < 1 || sig > NSIG) {
		errno = EINVAL;
		return(BADSIG);
	}
	old = sigdisp(sig);
	/*
	 * Does anyone actually call sigset with SIG_HOLD!?
	 */
	if (func == SIG_HOLD) {
		sighold(sig);
		return(old);
	}
	sigtable[sig].s_flag |= SSET;
	sigtable[sig].s_func = func;
	if (func == SIG_DFL) {
		/*
		 * If signal has been held, must retain
		 * the catch so that we can note occurrance
		 * of signal.
		 */
		if ((sigtable[sig].s_flag & SHELD) == 0)
			signal(sig, SIG_DFL);
		else
			signal(sig, _sigtramp);
		return(old);
	}
	if (func == SIG_IGN) {
		/*
		 * Clear pending signal
		 */
		signal(sig, SIG_IGN);
		sigtable[sig].s_flag &= ~SDEFER;
		return(old);
	}
	signal(sig, _sigtramp);
	return(old);
}

/*
 * Hold a signal.
 * This CAN be tricky if the signal's disposition is SIG_DFL.
 * In that case, we still catch the signal so we can note it
 * happened and do something crazy later.
 */
sigtype
sighold(sig)
{
	sigtype old;
	extern int errno;

	if (sig < 1 || sig > NSIG) {
		errno = EINVAL;
		return(BADSIG);
	}
	old = sigdisp(sig);
	if (sigtable[sig].s_flag & SHELD)
		return(old);
	/*
	 * When the default action is required, we have to
	 * set up to catch the signal to note signal's occurrance.
	 */
	if (old == SIG_DFL) {
		sigtable[sig].s_flag |= SSET;
		signal(sig, _sigtramp);
	}
	sigtable[sig].s_flag |= SHELD;
	return(old);
}

/*
 * Release a signal
 * If the signal occurred while we had it held, cause the signal.
 */
sigtype
sigrelse(sig)
{
	sigtype old;
	extern int errno;
	int _sigtramp();

	if (sig < 1 || sig > NSIG) {
		errno = EINVAL;
		return(BADSIG);
	}
	old = sigdisp(sig);
	if ((sigtable[sig].s_flag & SHELD) == 0)
		return(old);
	sigtable[sig].s_flag &= ~SHELD;
	if (sigtable[sig].s_flag & SDEFER)
		_sigtramp(sig);
	/*
	 * If disposition was the default, then we can unset the
	 * catch to _sigtramp() and let the system do the work.
	 */
	if (sigtable[sig].s_func == SIG_DFL)
		signal(sig, SIG_DFL);
	return(old);
}

/*
 * Ignore a signal.
 */
sigtype
sigignore(sig)
{

	return(sigset(sig, SIG_IGN));
}

/*
 * Pause, waiting for sig to occur.
 * We assume LUSER called us with the signal held.
 * When we got the signal, mark the signal as having
 * occurred.  It will actually cause something when
 * the signal is released.
 *
 * This is probably useless without job control anyway.
 */
sigpause(sig)
{
	extern int errno;

	if (sig < 1 || sig > NSIG) {
		errno = EINVAL;
		return;
	}
	sigtable[sig].s_flag |= SHELD|SPAUSE;
	if (setjmp(_pause) == 0)
		pause();
	sigtable[sig].s_flag &= ~SPAUSE;
	sigtable[sig].s_flag |= SDEFER;
}

/*
 * In the child process after fork(2), set the disposition of all held
 * signals to SIG_IGN.  This is a new procedure not in the real sigset()
 * package, provided for retrofitting purposes.
 */
sigchild()
{
	register int i;

	for (i = 1; i <= NSIG; i++)
		if (sigtable[i].s_flag & SHELD)
			signal(i, SIG_IGN);
}

/*
 * Return the current disposition of a signal
 * If we have not set this signal before, we have to
 * ask the system
 */
sigtype
sigdisp(sig)
{
	extern int errno;
	sigtype old;

	if (sig < 1 || sig > NSIG) {
		errno = EINVAL;
		return(BADSIG);
	}
	/*
	 * If we have no knowledge of this signal,
	 * ask the system, then save the result for later.
	 */
	if ((sigtable[sig].s_flag & SSET) == 0) {
		old = signal(sig, SIG_IGN);
		sigtable[sig].s_func = old;
		sigtable[sig].s_flag |= SSET;
		signal(sig, old);
		return(old);
	}
	/*
	 * If we have set this signal before, then sigset()
	 * will have been careful to leave something meaningful
	 * in s_func.
	 */
	return(sigtable[sig].s_func);
}

/*
 * The following routine gets called for any signal
 * that is to be trapped to a user function.
 */
_sigtramp(sig)
{
	extern int errno;
	sigtype old;

	if (sig < 1 || sig > NSIG) {
		errno = EINVAL;
		return;
	}

top:
	old = signal(sig, SIG_IGN);
	/*
	 * If signal being paused on, wakeup sigpause()
	 */
	if (sigtable[sig].s_flag & SPAUSE)
		longjmp(_pause, 1);
	/*
	 * If signal being held, mark its table entry
	 * so we can trigger it when signal released.
	 * Then just return.
	 */
	if (sigtable[sig].s_flag & SHELD) {
		sigtable[sig].s_flag |= SDEFER;
		signal(sig, _sigtramp);
		return;
	}
	/*
	 * If the signal is being ignored, just return.
	 * This would make SIGCONT more normal, but of course
	 * any system with SIGCONT also has the new signal pkg, so...
	 */
	if (sigtable[sig].s_func == SIG_IGN)
		return;
	/*
	 * If the signal is SIG_DFL, then we probably got here
	 * by holding the signal, having it happen, then releasing
	 * the signal.  I wonder if a process is allowed to send
	 * a signal to itself?
	 */
	if (sigtable[sig].s_func == SIG_DFL) {
		signal(sig, SIG_DFL);
		kill(getpid(), sig);
		/* Will we get back here? */
		return;
	}
	/*
	 * Looks like we should just cause the signal...
	 * We hold the signal for the duration of the user's
	 * code with the signal re-enabled.  If the signal
	 * happens again while in user code, we will recursively
	 * trap here and mark that we had another occurance
	 * and return to the user's trap code.  When we return
	 * from there, we can cause the signal again.
	 */
	sigtable[sig].s_flag &= ~SDEFER;
	sigtable[sig].s_flag |= SHELD;
	signal(sig, _sigtramp);
	(*sigtable[sig].s_func)(sig);
	/*
	 * If the signal re-occurred while in the user's routine,
	 * just go try it again...
	 */
	sigtable[sig].s_flag &= ~SHELD;
	if (sigtable[sig].s_flag & SDEFER)
		goto top;
}

/* Copyright (c) Stichting Mathematisch Centrum, Amsterdam, 1985. */

/*
  $Header: b3sig.c,v 1.4 85/08/27 10:56:21 timo Exp $
*/

/*Handle interrupts and signals*/

#include "b.h"
#include "b0fea.h"
#include "b1obj.h"
#include "b0con.h"
#include "b3scr.h"
#include "b3err.h"
#include "b3env.h"
#ifdef SETJMP
#include <setjmp.h>
#endif

#ifdef SIGNAL
#include <signal.h>
#endif

/*The operating system provides a function signal(s,f)
  that associates function f with the signal s, and returns
  a pointer to the previous function associated with s.
  Then, when signal s occurs, f is called and the function associated with s
  may or may not be reset. Thus f may need to call signal(s,f) again to.
  The code here doesn't depend on either interpretation, always being explicit
  about which handler to use.

  There are two signals that can come from the user: quit and interrupt.
  Interrupt should just stop the interpreter and return to B command level;
  quit should stop the B system completely.
  All other signals are caused by errors (eg memory exhausted)
  or come from outside the program, and are therefore fatal.

  SIG_IGN is the system supplied routine to ignore a signal.
  SIG_DFL is the system supplied default for a signal.
  kill(getpid(), signal) kills the program according to 'signal'

  On BSD systems, SIGTSTP and other signals causing the process to be
  suspended, and SIGCONT and others that are ignored by default,
  must not be caught.  It is assumed that all these are defined
  when SIGTSTP is defined.
*/

#ifdef SIGTSTP
Hidden bool must_handle(sig) int sig; {
	/* Shouldn't we enumerate the list of signals we *do* want to catch? */
	/* It seems that new signals are all of the type that should be
	   ignored by most processes... */
	switch (sig) {
	case SIGURG:
	case SIGSTOP:
	case SIGTSTP:
	case SIGCONT:
	case SIGCHLD:
	case SIGTTIN:
	case SIGTTOU:
	case SIGIO:
		return No;
	default:
		return Yes;
	}
}
#else
#ifdef SIGCLD /* System V */
#define must_handle(sig) ((sig) != SIGCLD)
#else
#define must_handle(sig) Yes
#endif
#endif

#ifdef NOT_USED
Visible Procedure dump() {
	if (cntxt != In_prmnv) putprmnv();
#ifdef KILL
	signal(SIGQUIT, SIG_DFL);
	kill(getpid(), SIGQUIT);
#else
	exit(-1);
#endif
}
#endif NOT_USED

#ifdef SIGNAL
Hidden Procedure oops(sig, m) int sig; string m; {
	signal(sig, SIG_DFL); /* Don't call handler recursive -- just die... */
#ifdef sigmask /* 4.2 BSD */
	sigsetmask(0); /* Don't block signals in handler -- just die... */
#endif
#ifdef EXT_COMMAND
	e_done();
#endif
	fflush(stdout);
	fprintf(stdout, "*** Oops, %s\n", m);
	fflush(stdout);
	if (cntxt != In_prmnv) putprmnv();
#ifdef KILL
	kill(getpid(), sig);
#else
	exit(-1);
#endif
}

Hidden Procedure burp(sig) int sig; {
	oops(sig,
 "I feel suddenly (BURP!) indisposed. I'll call it a day. Sorry.");
}

Hidden Procedure aog(sig) int sig; {
	oops(sig,
 "an act of God has occurred compelling me to discontinue service.");
}

Hidden Procedure fpe_signal(sig) int sig; {
	signal(sig /* == SIGFPE*/, fpe_signal);
	syserr(MESS(3900, "unexpected arithmetic overflow"));
}

#ifdef SETJMP
extern bool awaiting_input;
extern jmp_buf read_interrupt;
#endif

Hidden Procedure intsig(sig) int sig; { /*sig==SIGINT*/
	signal(sig, SIG_IGN);
	int_signal();
	signal(sig, intsig);
#ifdef SETJMP
	if (awaiting_input) longjmp(read_interrupt, 1);
#endif
}

#ifdef INTEGRATION

Visible Procedure bint_interrupt() {
	signal(SIGINT, intsig);
	if (interrupted) intsig(SIGINT);
}

#endif

Hidden int(* setsig(sig, func))() int sig, (*func)(); {
	/*Set a signal, unless it's being ignored*/
	int (*f)()= signal(sig, SIG_IGN);
	if (f != SIG_IGN) signal(sig, func);
	return f;
}
#endif

Visible Procedure initsig() {
#ifdef SIGNAL
	int i;
	for (i = 1; i<=NSIG; ++i)
		if (must_handle(i)) VOID setsig(i, burp);
#ifndef INTEGRATION
	if (filtered) {
		VOID setsig(SIGINT,  SIG_IGN);
		VOID setsig(SIGTRAP, intsig);
	} else {
		VOID setsig(SIGINT,  intsig);
		VOID setsig(SIGTRAP, burp);
	}
#else
	VOID setsig(SIGINT,  intsig);
#endif
	VOID setsig(SIGQUIT, aog);
	VOID setsig(SIGTERM, aog);
	VOID setsig(SIGFPE,  fpe_signal);
	VOID setsig(SIGPIPE, bye);
#endif SIGNAL
}

/* Copyright (c) Stichting Mathematisch Centrum, Amsterdam, 1984. */
/* $Header: b2sig.c,v 1.1 84/06/28 00:49:19 timo Exp $ */

/*Handle interrupts and signals*/
#include "b.h"
#include "b1obj.h"
#include "b0con.h"
#include "b2scr.h"
#include <signal.h>
#include "b2err.h"
#include "b2env.h"

/*The operating system provides a function signal(s,f)
  that associates function f with the signal s, and returns
  a pointer to the previous function associated with s.
  Then, when signal s occurs, f is called and the function associated with s
  may or may not be reset. Thus f may need to call signal(s,f) again to.
  The code here doesn't depend on either interpretation, always being explicit
  about which handler to use.

  There are two signals that can come from the user: quit and interrupt.
  Interrupt should just stop the interpreter and return to B command level;
  quit should stop the B system completely and produce a dump.
  All other signals are caused by errors (eg memory exhausted)
  or come from outside the program, and are therefore fatal.

  SIG_IGN is the system supplied routine to ignore a signal.
  SIG_DFL is the system supplied default for a signal.
  kill(getpid(), signal) kills the program according to 'signal'
*/

Visible Procedure dump() {
	signal(SIGQUIT, SIG_DFL);
	kill(getpid(), SIGQUIT);
}

Hidden Procedure oops(sig, m) int sig; string m; {
	fflush(stdout);
	fprintf(stdout, "*** Oops, %s\n", m);
	fflush(stdout);
	if (cntxt != In_prmnv) putprmnv();
	signal(sig, SIG_DFL);
	kill(getpid(), sig);
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
	error("arithmetic overflow");
}

Hidden Procedure intsig(sig) int sig; { /*sig==SIGINT*/
	signal(sig, SIG_IGN);
	int_signal(No);
}

Visible Procedure accept_int() {
	signal(SIGINT, intsig);
}

int (*si)(), (*sq)();
bool sawi= No, sawq= No;

Hidden Procedure signote(sig) int sig; {
	/*Note but otherwise ignore a quit or interrupt*/
	signal(sig, signote);
	fprintf(stderr, "*** Just a moment\n");
	if (sig == SIGINT) sawi= Yes;
	else if (sig == SIGQUIT) sawq= Yes;
}

Hidden int(* setsig(sig, func))() int sig, (*func)(); {
	/*Set a signal, unless it's being ignored*/
	int (*f)()= signal(sig, SIG_IGN);
	if (f != SIG_IGN) signal(sig, func);
	return f;
}

Visible Procedure ignsigs() {
	/*Henceforth only note quits and interrupts*/
	si= setsig(filtered ? SIGTRAP : SIGINT, signote);
	sq= setsig(SIGQUIT, signote);
}

Visible Procedure re_sigs() {
	/*Start processing quits and interrupts again*/
	signal(filtered ? SIGTRAP : SIGINT, si);
	signal(SIGQUIT, sq);
	if (sawi) {
		sawi= sawq= No;
		if (si != SIG_IGN && si != SIG_DFL) (*si)(filtered ? SIGTRAP : SIGINT);
	} else if (sawq) {
		sawq= No;
		if (sq != SIG_IGN && sq != SIG_DFL) (*sq)(SIGQUIT);
	}
}

Visible Procedure inisigs() {
	if (filtered) {
		VOID setsig(SIGINT, SIG_IGN);
		VOID setsig(SIGTRAP, intsig);
	}
	else {
		VOID setsig(SIGINT, intsig);
		VOID setsig(SIGTRAP, burp);
	}
	VOID setsig(SIGQUIT, aog);
	VOID setsig(SIGILL,  burp);
	VOID setsig(SIGIOT,  burp);
	VOID setsig(SIGEMT,  burp);
	VOID setsig(SIGFPE,  fpe_signal);
	VOID setsig(SIGBUS,  burp);
	VOID setsig(SIGSEGV, burp);
	VOID setsig(SIGSYS,  burp);
	VOID setsig(SIGPIPE, aog);
	VOID setsig(SIGALRM, burp);
	VOID setsig(SIGTERM, burp);
}

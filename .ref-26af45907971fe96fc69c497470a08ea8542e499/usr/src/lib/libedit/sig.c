/*-
 * Copyright (c) 1992, 1993
 *	The Regents of the University of California.  All rights reserved.
 *
 * This code is derived from software contributed to Berkeley by
 * Christos Zoulas of Cornell University.
 *
 * %sccs.include.redist.c%
 */

#if !defined(lint) && !defined(SCCSID)
static char sccsid[] = "@(#)sig.c	8.1 (Berkeley) %G%";
#endif /* not lint && not SCCSID */

/*
 * sig.c: Signal handling stuff.
 *	  our policy is to trap all signals, set a good state
 *	  and pass the ball to our caller.
 */
#include "sys.h"
#include "el.h"
#include <stdlib.h>

private EditLine *sel = NULL;

private int sighdl[] = {
#define _DO(a)	(a),
    ALLSIGS
#undef _DO
    -1
};

private void sig_handler	__P((int));

/* sig_handler():
 *	This is the handler called for all signals
 *	XXX: we cannot pass any data so we just store the old editline
 *	state in a private variable
 */
private void
sig_handler(signo)
    int signo;
{
    int i;
    sigset_t nset, oset;

    (void) sigemptyset(&nset);
    (void) sigaddset(&nset, signo);
    (void) sigprocmask(SIG_BLOCK, &nset, &oset);

    switch (signo) {
    case SIGCONT:
	tty_rawmode(sel);
	if (ed_redisplay(sel, 0) == CC_REFRESH)
	    re_refresh(sel);
	term__flush();
	break;

    case SIGWINCH:
	el_resize(sel);
	break;

    default:
	tty_cookedmode(sel);
	break;
    }

    for (i = 0; sighdl[i] != -1; i++) 
	if (signo == sighdl[i])
	    break;

    (void) signal(signo, sel->el_signal[i]);
    (void) sigprocmask(SIG_SETMASK, &oset, NULL);
    (void) kill(0, signo);
}


/* sig_init():
 *	Initialize all signal stuff
 */
protected int
sig_init(el)
    EditLine *el;
{
    int i;
    sigset_t nset, oset;

    (void) sigemptyset(&nset);
#define _DO(a) (void) sigaddset(&nset, SIGWINCH);
    ALLSIGS
#undef _DO
    (void) sigprocmask(SIG_BLOCK, &nset, &oset);

#define SIGSIZE (sizeof(sighdl) / sizeof(sighdl[0]) * sizeof(sig_t))

    el->el_signal = (sig_t *) el_malloc(SIGSIZE);
    for (i = 0; sighdl[i] != -1; i++) 
	el->el_signal[i] = BADSIG;

    (void) sigprocmask(SIG_SETMASK, &oset, NULL);

    return 0;
}


/* sig_end():
 *	Clear all signal stuff
 */
protected void
sig_end(el)
    EditLine *el;
{
    el_free((ptr_t) el->el_signal);
    el->el_signal = NULL;
}


/* sig_set():
 *	set all the signal handlers
 */
protected void
sig_set(el)
    EditLine *el;
{
    int i;
    sigset_t nset, oset;

    (void) sigemptyset(&nset);
#define _DO(a) (void) sigaddset(&nset, SIGWINCH);
    ALLSIGS
#undef _DO
    (void) sigprocmask(SIG_BLOCK, &nset, &oset);

    for (i = 0; sighdl[i] != -1; i++) {
	sig_t s;
	/* This could happen if we get interrupted */
	if ((s = signal(sighdl[i], sig_handler)) != sig_handler)
	    el->el_signal[i] = s;
    }
    sel = el;
    (void) sigprocmask(SIG_SETMASK, &oset, NULL);
}


/* sig_clr():
 *	clear all the signal handlers
 */
protected void
sig_clr(el)
    EditLine *el;
{
    int i;
    sigset_t nset, oset;

    (void) sigemptyset(&nset);
#define _DO(a) (void) sigaddset(&nset, SIGWINCH);
    ALLSIGS
#undef _DO
    (void) sigprocmask(SIG_BLOCK, &nset, &oset);

    for (i = 0; sighdl[i] != -1; i++) 
	if (el->el_signal[i] != BADSIG)
	    (void) signal(sighdl[i], el->el_signal[i]);

    sel = NULL;	/* we are going to die if the handler is called */
    (void) sigprocmask(SIG_SETMASK, &oset, NULL);
}

/* $Header: /home/hyperion/mu/christos/src/sys/tcsh-6.01/RCS/tc.sig.c,v 3.6 1991/11/11 01:56:34 christos Exp $ */
/*
 * sh.sig.c: Signal routine emulations
 */
/*-
 * Copyright (c) 1980, 1991 The Regents of the University of California.
 * All rights reserved.
 *
 * Redistribution and use in source and binary forms, with or without
 * modification, are permitted provided that the following conditions
 * are met:
 * 1. Redistributions of source code must retain the above copyright
 *    notice, this list of conditions and the following disclaimer.
 * 2. Redistributions in binary form must reproduce the above copyright
 *    notice, this list of conditions and the following disclaimer in the
 *    documentation and/or other materials provided with the distribution.
 * 3. All advertising materials mentioning features or use of this software
 *    must display the following acknowledgement:
 *	This product includes software developed by the University of
 *	California, Berkeley and its contributors.
 * 4. Neither the name of the University nor the names of its contributors
 *    may be used to endorse or promote products derived from this software
 *    without specific prior written permission.
 *
 * THIS SOFTWARE IS PROVIDED BY THE REGENTS AND CONTRIBUTORS ``AS IS'' AND
 * ANY EXPRESS OR IMPLIED WARRANTIES, INCLUDING, BUT NOT LIMITED TO, THE
 * IMPLIED WARRANTIES OF MERCHANTABILITY AND FITNESS FOR A PARTICULAR PURPOSE
 * ARE DISCLAIMED.  IN NO EVENT SHALL THE REGENTS OR CONTRIBUTORS BE LIABLE
 * FOR ANY DIRECT, INDIRECT, INCIDENTAL, SPECIAL, EXEMPLARY, OR CONSEQUENTIAL
 * DAMAGES (INCLUDING, BUT NOT LIMITED TO, PROCUREMENT OF SUBSTITUTE GOODS
 * OR SERVICES; LOSS OF USE, DATA, OR PROFITS; OR BUSINESS INTERRUPTION)
 * HOWEVER CAUSED AND ON ANY THEORY OF LIABILITY, WHETHER IN CONTRACT, STRICT
 * LIABILITY, OR TORT (INCLUDING NEGLIGENCE OR OTHERWISE) ARISING IN ANY WAY
 * OUT OF THE USE OF THIS SOFTWARE, EVEN IF ADVISED OF THE POSSIBILITY OF
 * SUCH DAMAGE.
 */
#include "sh.h"

RCSID("$Id: tc.sig.c,v 3.6 1991/11/11 01:56:34 christos Exp $")

#include "tc.wait.h"

#ifndef BSDSIGS

/* this stack is used to queue signals
 * we can handle up to MAX_CHLD outstanding children now;
 */
#define MAX_CHLD 50
static struct mysigstack {
    int     s_w;		/* wait report			 */
    int     s_errno;		/* errno returned;		 */
    pid_t   s_pid;		/* pid returned			 */
}       stk[MAX_CHLD];
static int stk_ptr = -1;


#ifdef UNRELSIGS
/* queue child signals
 */
static sigret_t
sig_ch_queue()
{
#ifdef JOBDEBUG
    xprintf("queue SIGCHLD\n");
    flush();
#endif /* JOBDEBUG */
    stk_ptr++;
    stk[stk_ptr].s_pid = (pid_t) wait(&stk[stk_ptr].s_w);
    stk[stk_ptr].s_errno = errno;
    (void) signal(SIGCHLD, sig_ch_queue);
#ifndef SIGVOID
    return(0);
#endif /* SIGVOID */
}

/* process all awaiting child signals
 */
static sigret_t
sig_ch_rel()
{
    while (stk_ptr > -1)
	pchild(SIGCHLD);
#ifdef JOBDEBUG
    xprintf("signal(SIGCHLD, pchild);\n");
#endif /* JOBDEBUG */
    (void) signal(SIGCHLD, pchild);
#ifndef SIGVOID
    return(0);
#endif /* SIGVOID */
}

/* libc.a contains these functions in SVID >= 3. */
sigret_t
(*sigset(a, b)) ()
    int     a;
    sigret_t  (*b) __P((int));
{
    return (signal(a, b));
}

/* release signal
 *	release all queued signals and
 *	set the default signal handler
 */
void
sigrelse(what)
    int     what;
{
    if (what == SIGCHLD)
	sig_ch_rel();

#ifdef notdef	/* XXX: Should not need that when compiled with SVID=1 */
# ifdef UNIXPC	
    if (what == SIGINT)
    	(void)signal(SIGINT, pintr);
# endif
#endif
}

/* hold signal
 * only works with child and interrupt
 */
void
sighold(what)
    int     what;
{
    if (what == SIGCHLD)
	(void) signal(SIGCHLD, sig_ch_queue);

#ifdef notdef	/* XXX: Should not need that when compiled with SVID=1 */
# ifdef UNIXPC	
    if (what == SIGINT)
    	(void)signal(SIGINT, SIG_IGN);
# endif
#endif
}

/* ignore signal
 */
void
sigignore(a)
    int     a;
{
    (void) signal(a, SIG_IGN);
}

/* atomically release one signal
 */
void
sigpause(what)
    int     what;
{
#ifdef notdef
    if (what == SIGCHLD) {
	if (stk_ptr > -1) {
	    pchild(SIGCHLD);
	}
	else {
	    (void) sleep(1);
	}
    }
#endif
    /* From: Jim Mattson <mattson%cs@ucsd.edu> */
    if (what == SIGCHLD)
	pchild(SIGCHLD);

}

#endif /* UNRELSIGS */

#ifdef SXA
/*
 * SX/A is SVID3 but does not have sys5-sigpause().
 * I've heard that sigpause() is not defined in SVID3.
 */
/* This is not need if you make tcsh by BSD option's cc. */
void
sigpause(what)
{
    if (what == SIGCHLD) {
	bsd_sigpause(bsd_sigblock((sigmask_t) 0) & ~sigmask(SIGBSDCHLD));
    }
    else if (what == 0) {
	pause();
    }
    else {
	xprintf("sigpause(%d)\n", what);
	pause();
    }
}

#endif /* SXA */

/* return either awaiting processes or do a wait now
 */
pid_t
ourwait(w)
    int    *w;
{
    pid_t pid;

#ifdef JOBDEBUG
    xprintf("our wait %d\n", stk_ptr);
    flush();
#endif /* JOBDEBUG */

    if (stk_ptr == -1) {
	/* stack empty return signal from stack */
	pid = (pid_t) wait(w);
#ifdef JOBDEBUG
	xprintf("signal(SIGCHLD, pchild);\n");
#endif /* JOBDEBUG */
	(void) signal(SIGCHLD, pchild);
	return (pid);
    }
    else {
	/* return signal from stack */
	errno = stk[stk_ptr].s_errno;
	*w = stk[stk_ptr].s_w;
	stk_ptr--;
	return (stk[stk_ptr + 1].s_pid);
    }
} /* end ourwait */

#endif /* BSDSIGS */

#ifdef NEEDsignal
/* turn into bsd signals */
sigret_t(*
	 xsignal(s, a)) ()
    int     s;
    sigret_t (*a) __P((int));
{
    sigvec_t osv, sv;

    (void) mysigvec(s, NULL, &osv);
    sv = osv;
    sv.sv_handler = a;
#ifdef SIG_STK
    sv.sv_onstack = SIG_STK;
#endif
#ifdef SV_BSDSIG
    sv.sv_flags = SV_BSDSIG;
#endif

    if (mysigvec(s, &sv, NULL) < 0)
	return (BADSIG);
    return (osv.sv_handler);
}

#endif /* NEEDsignal */

#ifdef _SEQUENT_
/*
 * Support for signals.
 */

extern int errno;

/* Set and test a bit.  Bits numbered 1 to 32 */

#define SETBIT(x, y)	x |= sigmask(y)
#define ISSET(x, y)	((x & sigmask(y)) != 0)

#ifdef DEBUG
# define SHOW_SIGNALS	1	/* to assist in debugging signals */
#endif

#ifdef SHOW_SIGNALS
char   *show_sig_mask();
#endif

int     debug_signals = 0;

/*
 * igsetmask(mask)
 *
 * Set a new signal mask.  Return old mask.
 */
sigmask_t
sigsetmask(mask)
    sigmask_t     mask;
{
    sigset_t set, oset;
    int     m;
    register int i;

    sigemptyset(&set);
    sigemptyset(&oset);

    for (i = 1; i <= MAXSIG; i++)
	if (ISSET(mask, i))
	    sigaddset(&set, i);

    if (sigprocmask(SIG_SETMASK, &set, &oset))
	xprintf("sigsetmask(0x%x) - sigprocmask failed, errno %d",
		mask, errno);

    m = 0;
    for (i = 1; i < MAXSIG; i++)
	if (sigismember(&oset, i))
	    SETBIT(m, i);

    return (m);
}

/*
 * sigblock(mask)
 *
 * Add "mask" set of signals to the present signal mask.
 * Return old mask.
 */
sigmask_t
sigblock(mask)
    sigmask_t     mask;
{
    sigset_t set, oset;
    int     m;
    register int i;

    set = 0;
    oset = 0;

    /* Get present set of signals. */
    if (sigprocmask(SIG_SETMASK, NULL, &set))
	xprintf("sigblock(0x%x) - sigprocmask failed, errno %d",
		mask, errno);

    /* Add in signals from mask. */
    for (i = 1; i <= MAXSIG; i++)
	if (ISSET(mask, i))
	    sigaddset(&set, i);

    sigprocmask(SIG_SETMASK, &set, &oset);

    /* Return old mask to user. */
    m = 0;
    for (i = 1; i < MAXSIG; i++)
	if (sigismember(&oset, i))
	    SETBIT(m, i);

    return (m);
}


/*
 * bsd_sigpause(mask)
 *
 * Set new signal mask and wait for signal;
 * Old mask is restored on signal.
 */
void
bsd_sigpause(mask)
    sigmask_t     mask;
{
    sigset_t set;
    register int i;

    sigemptyset(&set);

    for (i = 1; i <= MAXSIG; i++)
	if (ISSET(mask, i))
	    sigaddset(&set, i);
    sigsuspend(&set);
}
#endif /* _SEQUENT_ */


#ifdef SIGSYNCH
static long Synch_Cnt = 0;

sigret_t
synch_handler(sno)
int sno;
{
    if (sno != SIGSYNCH)
	abort();
    Synch_Cnt++;
}
#endif /* SIGSYNCH */

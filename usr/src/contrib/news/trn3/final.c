/* $Id: final.c,v 3.0 1992/02/01 03:09:32 davison Trn $
 */
/* This software is Copyright 1991 by Stan Barber. 
 *
 * Permission is hereby granted to copy, reproduce, redistribute or otherwise
 * use this software as long as: there is no monetary profit gained
 * specifically from the use or reproduction of this software, it is not
 * sold, rented, traded or otherwise marketed, and this copyright notice is
 * included prominently in any copy made. 
 *
 * The author make no claims as to the fitness or correctness of this software
 * for any use whatsoever, and it is provided as is. Any use of this software
 * is at the user's own risk. 
 */

#include "EXTERN.h"
#include "common.h"
#include "util.h"
#include "cache.h"
#include "bits.h"
#include "term.h"
#include "ng.h"
#include "init.h"
#include "last.h"
#include "rcstuff.h"
#include "ngdata.h"
#include "artio.h"
#include "intrp.h"
#include "nntp.h"
#include "INTERN.h"
#include "final.h"

#ifndef sigmask
#define sigmask(m)	(1 << ((m)-1))
#endif

void
final_init()
{
#ifdef SIGTSTP
    sigset(SIGTSTP, stop_catcher);	/* job control signals */
    sigset(SIGTTOU, stop_catcher);	/* job control signals */
    sigset(SIGTTIN, stop_catcher);	/* job control signals */
#endif

    sigset(SIGINT, int_catcher);	/* always catch interrupts */
#ifdef SIGHUP
    sigset(SIGHUP, sig_catcher);	/* and hangups */
#endif
#ifdef SIGWINCH
    sigset(SIGWINCH, winch_catcher);
#endif

#ifndef lint
#ifdef SIGEMT
    sigignore(SIGEMT);
#endif
#endif

#ifdef DEBUG
    /* sometimes we WANT a core dump */
    if (debug & DEB_COREDUMPSOK)
	return;
#endif
    sigset(SIGILL, sig_catcher);
#ifdef SIGTRAP
    sigset(SIGTRAP, sig_catcher);
#endif
    sigset(SIGFPE, sig_catcher);
#ifdef SIGBUS
    sigset(SIGBUS, sig_catcher);
#endif
    sigset(SIGSEGV, sig_catcher);
#ifdef SIGSYS
    sigset(SIGSYS, sig_catcher);
#endif
    sigset(SIGTERM, sig_catcher);
#ifdef SIGXCPU
    sigset(SIGXCPU, sig_catcher);
#endif
#ifdef SIGXFSZ
    sigset(SIGXFSZ, sig_catcher);
#endif
}

void					/* very much void */
finalize(status)
int status;
{
    termlib_reset();
    if (bizarre)
	resetty();
    if (lockname && *lockname)
 	UNLINK(lockname);
#ifdef USE_NNTP
    nntp_cleanup();
#endif
    if (status < 0) {
	chdir("/usr/tmp");
	sigset(SIGILL,SIG_DFL);
#ifdef HAS_SIGBLOCK
	sigsetmask(sigblock(0) & ~(sigmask(SIGILL) | sigmask(SIGIOT)));
#endif
	abort();
    }
    exit(status);
}

/* come here on interrupt */

Signal_t
int_catcher(dummy)
int dummy;
{
    sigset(SIGINT,int_catcher);
#ifdef DEBUG
    if (debug)
	write(2,"int_catcher\n",12);
#endif
    if (!waiting) {
	if (int_count) {		/* was there already an interrupt? */
	    write(2,"\nBye-bye.\n",10);
	    sig_catcher(0);		/* emulate the other signals */
	}
	int_count++;
    }
}

/* come here on signal other than interrupt, stop, or cont */

Signal_t
sig_catcher(signo)
int signo;
{
#ifdef VERBOSE
    static char *signame[] = {
	"",
	"HUP",
	"INT",
	"QUIT",
	"ILL",
	"TRAP",
	"IOT",
	"EMT",
	"FPE",
	"KILL",
	"BUS",
	"SEGV",
	"SYS",
	"PIPE",
	"ALRM",
	"TERM",
	"???"
#ifdef SIGTSTP
	,"STOP",
	"TSTP",
	"CONT",
	"CHLD",
	"TTIN",
	"TTOU",
	"TINT",
	"XCPU",
	"XFSZ"
#ifdef SIGPROF
	,"VTALARM",
	"PROF"
#endif
#endif
	};
#endif

#ifdef DEBUG
    if (debug) {
	printf("\nSIG%s--.newsrc not restored in debug\n",signame[signo]);
	finalize(-1);
    }
#endif
    if (panic) {
#ifdef HAS_SIGBLOCK
      sigsetmask(sigblock(0) & ~(sigmask(SIGILL) | sigmask(SIGIOT)));
#endif
	abort();
      }
    (void) sigset(SIGILL,SIG_DFL);
    panic = TRUE;			/* disable terminal I/O */
    if (doing_ng) {			/* need we reconstitute rc line? */
	yankback();
	bits_to_rc();			/* then do so (hope this works) */
    }
    doing_ng = FALSE;
    if (rc_changed)			/* need we write .newsrc out? */
	write_rc();			/* then do so */
    rc_changed = FALSE;
#ifdef SIGHUP
    if (signo != SIGHUP)
#endif
#ifdef VERBOSE
	IF(verbose)
	    printf("\nCaught %s%s--.newsrc restored\n",
		signo ? "a SIG" : "an internal error", signame[signo]);
	ELSE
#endif
#ifdef TERSE
	    printf("\nSignal %d--bye bye\n",signo);
#endif
    switch (signo) {
#ifdef SIGBUS
    case SIGBUS:
#endif
    case SIGILL:
    case SIGSEGV:
	finalize(-signo);
    }
    finalize(1);				/* and blow up */
}

#ifdef SIGTSTP
/* come here on stop signal */

Signal_t
stop_catcher(signo)
int signo;
{
    if (!waiting) {
	checkpoint_rc();	/* good chance of crash while stopped */
	if (clear_on_stop) {
	    clear();
	    putchar('\n') FLUSH;
	}
	termlib_reset();
	resetty();		/* this is the point of all this */
#ifdef DEBUG
	if (debug)
	    write(2,"stop_catcher\n",13);
#endif
	sigset(signo,SIG_DFL);	/* enable stop */
#ifdef HAS_SIGBLOCK
	sigsetmask(sigblock(0) & ~(1 << (signo-1)));
#endif
	kill(0,signo);		/* and do the stop */
    	savetty();
#ifdef MAILCALL
    	mailcount = 0;			/* force recheck */
#endif
    	if (!panic) {
	    if (!waiting) {
		termlib_init();
	    	noecho();			/* set no echo */
	    	crmode();			/* set cbreak mode */
	    	forceme("\f");		/* cause a refresh */
					/* (defined only if TIOCSTI defined) */
		errno = 0;			/* needed for getcmd */
	    }
    	}
    }
    sigset(signo,stop_catcher);	/* unenable the stop */
}
#endif

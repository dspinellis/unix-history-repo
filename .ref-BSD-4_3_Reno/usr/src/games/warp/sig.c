/* $Header: /usr/src/games/warp/RCS/sig.c,v 1.1 87/07/03 01:47:11 games Exp $ */

/* $Log:	sig.c,v $
 * Revision 7.0.1.1a  87/07/03  01:47:11  games
 * Changed sigsetmask to use sigmask instead of calculating it (incorrectly)
 * by hand.
 * 
 * Revision 7.0.1.1  86/12/12  17:02:44  lwall
 * Baseline for net release.
 * 
 * Revision 7.0  86/10/08  15:13:24  lwall
 * Split into separate files.  Added amoebas and pirates.
 * 
 */

#include "EXTERN.h"
#include "warp.h"
#include "play.h"
#include "score.h"
#include "term.h"
#include "util.h"
#include "INTERN.h"
#include "sig.h"

void
sig_init()
{
#ifdef lint
    ;
#else
    sigignore(SIGINT);  /* for inquiry of existence via kill call */
#ifdef SIGTTOU
    sigignore(SIGTTOU);
#endif

    sigset(SIGHUP, sig_catcher);
    if (!debugging) {
	sigset(SIGQUIT, sig_catcher);
	sigset(SIGILL, sig_catcher);
	sigset(SIGFPE, sig_catcher);
	sigset(SIGBUS, sig_catcher);
	sigset(SIGSEGV, sig_catcher);
	sigset(SIGSYS, sig_catcher);
	sigset(SIGTERM, sig_catcher);
    }
#ifdef SIGXCPU
    sigset(SIGXCPU, sig_catcher);
#endif
#ifdef SIGCONT
    sigset(SIGCONT, cont_catcher);
#endif
#ifdef SIGTSTP
    sigset(SIGTSTP, stop_catcher);
    sigset(SIGSTOP, stop_catcher);
#endif
#endif /* lint */
}

#ifdef SIGTSTP
void
cont_catcher()
{
#ifndef lint
    sigset(SIGCONT,cont_catcher);
#endif
    savetty();
    crmode();
    raw();
    noecho();
    nonl();
}
#endif

void
mytstp()
{
    resetty();
#ifdef SIGTSTP
    kill(0,SIGTSTP);
#else
    if (fork())
	wait(0);
    else {
	char *shell = getenv("SHELL");

	setuid(getuid());
	if (!*shell)
	    shell = "/bin/sh";
	execl(shell,shell,0);
	exit(1);
    }
#endif
    rewrite();
}

void					/* very much void */
finalize(status)
int status;
{
    if (bizarre)
	resetty();
    if (status < 0) {
	chdir("/usr/tmp");
	sigset(SIGILL,SIG_DFL);
	abort();
    }
    exit(status);
}

/* come here on signal other than interrupt, stop, or cont */

void
sig_catcher(signo)
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

#ifdef SIGTTOU
#ifndef lint
    sigignore(SIGTTOU);
#endif /* lint */
#endif
#ifdef DEBUGGING
    if (debug) {
	printf("\r\nSIG%s--game not saved in debug\r\n",signame[signo]);
	finalize(-1);
    }
#endif
    panic++;
    if (panic >= 2) {
	if (panic >= 3)
	    abort();
	chdir(SAVEDIR);
	kill(0,SIGIOT);
    }
    (void) sigset(SIGILL,SIG_DFL);
    if (signo == SIGHUP && (timer < 10 || didkill))
	signo = SIGQUIT;
    if (signo == SIGQUIT) {	/* can't let them bomb out without penalty */
	if (smarts < 20)
	    smarts += 4;
	else if (smarts < 35)
	    smarts += 2;
	else
	    smarts++;
	totalscore -= possiblescore / 2;
    }
    save_game();
    if (signo != SIGHUP && signo != SIGQUIT)
#ifdef VERBOSE
	IF(verbose)
	    printf("\r\nCaught %s%s--%s\r\n",
		signo ? "a SIG" : "an internal error", signame[signo],
		experimenting ? "game saved" : "bye bye");
	ELSE
#endif
#ifdef TERSE
	    printf("\r\nSignal %d--bye bye\r\n",signo);
#endif
    switch (signo) {
    case SIGBUS:
    case SIGILL:
    case SIGSEGV:
	finalize(-signo);
    }
    finalize(1);				/* and blow up */
}

#ifdef SIGTSTP
/* come here on stop signal */

void
stop_catcher()
{
    if (!waiting) {
	resetty();			/* this is the point of all this */
#ifdef DEBUGGING
	if (debug)
	    write(2,"stop_catcher\r\n",13);
#endif
	sigset(SIGTSTP,SIG_DFL);	/* enable stop */
#ifdef BSD42
	sigsetmask(sigblock(0L) & ~sigmask(SIGTSTP));
#endif
	kill(0,SIGTSTP);		/* and do the stop */
    }
#ifndef lint
    sigset(SIGTSTP,stop_catcher);	/* unenable the stop */
#endif
}
#endif

/* pidwait.c - wait for child to exit */
#ifndef	lint
static char ident[] = "@(#)$Id: pidwait.c,v 1.10 1992/12/15 00:20:22 jromine Exp $";
#endif	/* lint */

#include "../h/mh.h"
#include <signal.h>
#include <stdio.h>
#if defined (BSD42) || defined (SVR4)
#include <sys/wait.h>
#endif


int     pidwait (id, sigsok)
register int     id,
		 sigsok;
{
    register int    pid;
    TYPESIG (*hstat) (), (*istat) (), (*qstat) (), (*tstat) ();
#if	defined(BSD42) && !defined(WAITINT)
    union wait status;
#else
    int     status;
#endif

    if (sigsok == NOTOK) {
#ifdef	notdef		/* I don't see why to trap these... */
	hstat = signal (SIGHUP, SIG_IGN);
	tstat = signal (SIGTERM, SIG_IGN);
#endif
	istat = signal (SIGINT, SIG_IGN);
	qstat = signal (SIGQUIT, SIG_IGN);
    }

#ifdef	SVR4
    pid = waitpid (id, &status, WUNTRACED);
#else
    while ((pid = wait (&status)) != NOTOK && pid != id)
	continue;
#endif

    if (sigsok == NOTOK) {
#ifdef	notdef
	(void) signal (SIGHUP, hstat);
	(void) signal (SIGTERM, tstat);
#endif
	(void) signal (SIGINT, istat);
	(void) signal (SIGQUIT, qstat);
    }

#if defined(BSD42) && !defined(WAITINT)
    return (pid == NOTOK ? NOTOK : status.w_status);
#else
    return (pid == NOTOK ? NOTOK : status);
#endif
}

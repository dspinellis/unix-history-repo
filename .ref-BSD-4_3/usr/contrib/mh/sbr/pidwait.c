/* pidwait.c - wait for child to exit */

#include "../h/mh.h"
#include <signal.h>
#include <stdio.h>
#ifdef	BSD42
#include <sys/wait.h>
#endif	BSD42


int     pidwait (id, sigsok)
register int     id,
		 sigsok;
{
    register int    pid;
#ifndef	BSD42
    int     status;
#else	BSD42
    union wait status;
#endif	BSD42
    int     (*hstat) (), (*istat) (), (*qstat) (), (*tstat) ();

    if (sigsok == NOTOK) {
	hstat = signal (SIGHUP, SIG_IGN);
	istat = signal (SIGINT, SIG_IGN);
	qstat = signal (SIGQUIT, SIG_IGN);
	tstat = signal (SIGTERM, SIG_IGN);
    }

    while ((pid = wait (&status)) != NOTOK && pid != id)
	continue;

    if (sigsok == NOTOK) {
	(void) signal (SIGHUP, hstat);
	(void) signal (SIGINT, istat);
	(void) signal (SIGQUIT, qstat);
	(void) signal (SIGTERM, tstat);
    }

#ifndef	BSD42
    return (pid == NOTOK ? NOTOK : status);
#else	BSD42
    return (pid == NOTOK ? NOTOK : status.w_status);
#endif	BSD42
}

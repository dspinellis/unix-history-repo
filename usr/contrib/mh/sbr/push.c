/* push.c - push a fork into the background */

#include "../h/mh.h"
#include <stdio.h>
#include <signal.h>


void	push () {
    register int     i;

    for (i = 0; i < 5; i++) {
	switch (fork ()) {
	    case NOTOK: 
		sleep (5);
		continue;

	    case OK: 
		(void) signal (SIGHUP, SIG_IGN);
		(void) signal (SIGINT, SIG_IGN);
		(void) signal (SIGQUIT, SIG_IGN);
		(void) signal (SIGTERM, SIG_IGN);
#ifdef	SIGTSTP
		(void) signal (SIGTSTP, SIG_IGN);
		(void) signal (SIGTTIN, SIG_IGN);
		(void) signal (SIGTTOU, SIG_IGN);
#endif	SIGTSTP
		(void) freopen ("/dev/null", "r", stdin);
		(void) freopen ("/dev/null", "w", stdout);
		break;

	    default: 
		done (0);
	}
	break;
    }
    if (i >= 5)
	advise (NULLCP, "unable to fork, so can't push...");
}

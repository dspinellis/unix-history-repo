/* pidstatus.c - report child's status */
#ifndef	lint
static char ident[] = "@(#)$Id: pidstatus.c,v 1.4 1993/02/26 21:57:34 jromine Exp $";
#endif	/* lint */

#include "../h/mh.h"
#include <signal.h>
#include <stdio.h>


#ifndef	BSD44
#ifndef	BSD42
static char *sigs[] = {
    NULL,
    "Hangup",
    "Interrupt",
    "Quit",
    "Illegal instruction",
    "Trace/BPT trap",
    "IOT trap",
    "EMT trap",
    "Floating exception",
    "Killed",
    "Bus error",
    "Segmentation fault",
    "Bad system call",
    "Broken pipe",
    "Alarm clock",
    "Terminated",
#ifdef	SIGURG
    "Urgent I/O condition",
#else
    NULL,
#endif
    "Stopped (signal)",
    "Stopped",
    "Continued",
    "Child exited",
    "Stopped (tty input)",
    "Stopped (tty output)",
    "Tty input interrupt",
    "Cputime limit exceeded",
    "Filesize limit exceeded",
    NULL
};
#else
extern  char *sys_siglist[];
#endif	/* BSD42 */
#endif	/* BSD44 defines sys_siglist in signal.h */

/*  */

int	pidstatus (status, fp, cp)
register int   status;
register FILE *fp;
register char *cp;
{
    int     signum;

    if ((status & 0xff00) == 0xff00)
	return status;

    switch (signum = status & 0x007f) {
	case OK: 
	    if (signum = ((status & 0xff00) >> 8)) {
		if (cp)
		    fprintf (fp, "%s: ", cp);
		fprintf (fp, "Exit %d\n", signum);
	    }
	    break;

	case SIGINT: 
	    break;

	default: 
	    if (cp)
		fprintf (fp, "%s: ", cp);
#ifndef	BSD42
	    if (signum >= sizeof sigs || sigs[signum] == NULL)
		fprintf (fp, "Signal %d", signum);
	    else
		fprintf (fp, "%s", sigs[signum]);
#else	/* BSD42 */
	    if (signum >= NSIG)
		fprintf (fp, "Signal %d", signum);
	    else
		fprintf (fp, "%s", sys_siglist[signum]);
#endif	/* BSD42 */
	    fprintf (fp, "%s\n", status & 0x80 ? " (core dumped)" : "");
	    break;
    }

    return status;
}

/*  $Revision: 1.11 $
**
**  Process control routines.
*/
#include "innd.h"
#include <signal.h>


STATIC PROCESS	*PROCtable;
STATIC int	PROCtablesize;
STATIC PROCESS	PROCnull = { PSfree };


/*
**  Collect dead processes.
*/
STATIC void
PROCreap()
{
    int			status;
    register PROCESS	*pp;
    register int	i;
    register int	pid;

    for ( ; ; ) {
	pid = waitnb(&status);
	if (pid == 0)
	    break;
	if (pid < 0) {
	    if (errno != ECHILD)
		syslog(L_ERROR, "%s cant wait %m", LogName);
	    break;
	}
	for (pp = PROCtable, i = PROCtablesize; --i >= 0; pp++)
	    if (pp->Pid == pid) {
		PROCneedscan = TRUE;
		pp->Status = status;
		pp->State = PSdead;
		pp->Collected = Now.time;
		break;
	    }
    }
}


/*
**  Signal handler that collects the processes, then resets the signal.
*/
STATIC SIGHANDLER
PROCcatchsignal(s)
    int			s;
{
    PROCreap();
    (void)signal(s, PROCcatchsignal);
}


/*
**  Synchronous version that notifies a site when its process went away.
*/
void
PROCscan()
{
    register PROCESS	*pp;
    register int	i;

    for (pp = PROCtable, i = PROCtablesize; --i >= 0; pp++)
	if (pp->State == PSdead) {
	    if (pp->Site > 0)
		SITEprocdied(&Sites[pp->Site], pp - PROCtable, pp);
	    pp->State = PSfree;
	}
    PROCneedscan = FALSE;
}


#if	0
/*
**  Close down all processes.
*/
void
PROCclose(Quickly)
    BOOL		Quickly;
{
    register int	sig;
    register PROCESS	*pp;
    register int	i;

    /* What signal are we sending? */
    sig = Quickly ? SIGKILL : SIGTERM;

    /* Send the signal to all living processes. */
    for (pp = PROCtable, i = PROCtablesize; --i >= 0; pp++) {
	if (pp->State != PSrunning)
	    continue;
	if (kill(pp->Pid, sig) < 0 && errno != ESRCH)
	    syslog(L_ERROR, "%s cant kill %s %d %m",
		LogName, Quickly ? "KILL" : "TERM", pp->Pid);
    }

    /* Collect any who might have died. */
    PROCreap();
    for (pp = PROCtable, i = PROCtablesize; --i >= 0; pp++)
	if (pp->State == PSdead)
	    *pp = PROCnull;
}
#endif	/* 0 */


/*
**  Stop watching a process -- we don't care about it any more.
*/
void
PROCunwatch(process)
    int		process;
{
    if (process < 0 || process >= PROCtablesize) {
	syslog(L_ERROR, "%s internal PROCunwatch %d", LogName, process);
	return;
    }
    PROCtable[process].Site = -1;
}


/*
**  Add a pid to the list of processes we watch.
*/
int
PROCwatch(pid, site)
    int			pid;
    int			site;
{
    register PROCESS	*pp;
    register int	i;

    /* Find a free slot for this process. */
    for (pp = PROCtable, i = PROCtablesize; --i >= 0; pp++)
	if (pp->State == PSfree)
	    break;
    if (i < 0) {
	/* Ran out of room -- grow the table. */
	RENEW(PROCtable, PROCESS, PROCtablesize + 20);
	pp = &PROCtable[PROCtablesize];
	PROCtablesize += 20;
    }

    pp->State = PSrunning;
    pp->Pid = pid;
    pp->Started = Now.time;
    pp->Site = site;
    return pp - PROCtable;
}


/*
**  Setup.
*/
void
PROCsetup(i)
    register int	i;
{
    register PROCESS	*pp;

    if (PROCtable)
	DISPOSE(PROCtable);
    PROCtablesize = i;
    PROCtable = NEW(PROCESS, PROCtablesize);
    for (pp = PROCtable, i = PROCtablesize; --i >= 0; pp++)
	*pp = PROCnull;

#if	defined(SIGCHLD)
    (void)signal(SIGCHLD, PROCcatchsignal);
#endif	/* defined(SIGCHLD) */
    (void)signal(SIGPIPE, PROCcatchsignal);
}

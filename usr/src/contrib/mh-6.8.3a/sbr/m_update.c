/* m_update.c - update the profile */
#ifndef	lint
static char ident[] = "@(#)$Id: m_update.c,v 1.7 1992/12/15 00:20:22 jromine Exp $";
#endif	lint

#include "../h/mh.h"
#include <stdio.h>
#include <signal.h>
#ifndef	sigmask
#define	sigmask(s)	(1 << ((s) - 1))
#endif	/* not sigmask */

static int	m_chkids();

void m_update () {
    int     action;
#ifndef	BSD42
    TYPESIG (*hstat) (), (*istat) (), (*qstat) (), (*tstat) ();
#else	/* BSD42 */
    int	    smask;
#endif	/* BSD42 */
    register struct node   *np;
    FILE * out;

    if (!(ctxflags & CTXMOD))
	return;
    ctxflags &= ~CTXMOD;

    if ((action = m_chkids ()) > OK)
	return;			/* child did it for us */

#ifndef	BSD42
    hstat = signal (SIGHUP, SIG_IGN);
    istat = signal (SIGINT, SIG_IGN);
    qstat = signal (SIGQUIT, SIG_IGN);
    tstat = signal (SIGTERM, SIG_IGN);
#else	/* BSD42 */
    smask = sigblock (sigmask (SIGHUP) | sigmask (SIGINT)
			| sigmask (SIGQUIT) | sigmask (SIGTERM));
#endif	/* BSD42 */

    if ((out = fopen (ctxpath, "w")) == NULL)
	adios (ctxpath, "unable to write");
    for (np = m_defs; np; np = np -> n_next)
	if (np -> n_context)
	    fprintf (out, "%s: %s\n", np -> n_name, np -> n_field);
    (void) fclose (out);

#ifndef	BSD42
    (void) signal (SIGHUP, hstat);
    (void) signal (SIGINT, istat);
    (void) signal (SIGQUIT, qstat);
    (void) signal (SIGTERM, tstat);
#else	/* BSD42 */
    (void) sigsetmask (smask);
#endif	/* BSD42 */
    if (action == OK)
	_exit (0);		/* we are child, time to die */
}

/*  */

/* This hack brought to you so we can handle set[ug]id MH programs.  If we
   return NOTOK, then no fork is made, we update .mh_profile normally, and
   return to the caller normally.  If we return 0, then the child is
   executing, .mh_profile is modified after we set our [ug]ids to the norm.
   If we return > 0, then the parent is executed and .mh_profile has
   already be modified.  We can just return to the caller immediately. */


static int  m_chkids () {
    int     i,
            child_id;

    if (getuid () == geteuid ())
	return (NOTOK);

    for (i = 0; (child_id = fork ()) == -1 && i < 5; i++)
	sleep (5);
    switch (child_id) {
	case NOTOK:
	    break;

	case OK:
	    (void) setgid (getgid ());
	    (void) setuid (getuid ());
	    break;

	default:
	    (void) pidwait (child_id, NOTOK);
	    break;
    }

    return child_id;
}

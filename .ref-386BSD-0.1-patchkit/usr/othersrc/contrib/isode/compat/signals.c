/* signals.c - signal handling */

#ifndef	lint
static char *rcsid = "$Header: /f/osi/compat/RCS/signals.c,v 7.2 91/02/22 09:15:51 mrose Interim $";
#endif

/* 
 * $Header: /f/osi/compat/RCS/signals.c,v 7.2 91/02/22 09:15:51 mrose Interim $
 *
 *
 * $Log:	signals.c,v $
 * Revision 7.2  91/02/22  09:15:51  mrose
 * Interim 6.8
 * 
 * Revision 7.1  90/10/29  18:38:03  mrose
 * updates
 * 
 * Revision 7.0  89/11/23  21:23:28  mrose
 * Release 6.0
 * 
 */

/*
 *				  NOTICE
 *
 *    Acquisition, use, and distribution of this module and related
 *    materials are subject to the restrictions of a license agreement.
 *    Consult the Preface in the User's Manual for the full terms of
 *    this agreement.
 *
 */


/* LINTLIBRARY */

#include <signal.h>
#ifndef	BADSIG
#define	BADSIG	((SFP) -1)
#endif
#include "manifest.h"

/*  */

int	_iosignals_set = 0;

/*    Berkeley UNIX: 4.2 */

#ifndef	XOS_2
#ifdef	BSDSIGS

/* Simply including <signal.h> is sufficient for everything but AIX */

#ifdef	AIX		/* #define'd to be _signal */
IFP	signal (sig, func)
int	sig;
IFP	func;
{
    struct sigvec   sv1,
    		    sv2;

    sv1.sv_handler = func;
    sv1.sv_mask = sv1.sv_onstack = 0;
    return (sigvec (sig, &sv1, &sv2) != NOTOK ? sv2.sv_handler : BADSIG);
}
#endif

#else

/*    AT&T UNIX: 5 */


/* Probably a race condition or two in this code */


static int blocked = 0;
static int pending = 0;

static SFP handler[NSIG];


static int sigser (sig)
int	sig;
{
    (void) signal (sig, sigser);

    pending |= sigmask (sig);
}

/*  */

int	sigblock (mask)
int	mask;
{
    register int    sig,
                    smask;
    long    omask = blocked;

    if (mask == 0)
	return blocked;

    for (sig = 1, smask = sigmask (sig); sig < NSIG; sig++, smask <<= 1)
	if ((smask & mask) && !(smask & blocked)) {
	    pending &= ~smask;
	    handler[sig] = signal (sig, sigser);
	    blocked |= smask;
	}

    return omask;
}


int	sigsetmask (mask)
int	mask;
{
    register int    sig,
                    smask;
    long    omask = blocked;

    for (sig = 1, smask = sigmask (sig); sig < NSIG; sig++, smask <<= 1)
	if (smask & mask) {
	    if (smask & blocked)
		continue;

	    pending &= ~smask;
	    handler[sig] = signal (sig, sigser);
	    blocked |= smask;
	}
	else
	    if (smask & blocked) {
		blocked &= ~smask;
		(void) signal (sig, handler[sig] != BADSIG ? handler[sig]
			: SIG_DFL);
		if (smask & pending) {
		    pending &= ~smask;
		    (void) kill (getpid (), sig);
		}
	    }

    return omask;
}

#endif
#endif

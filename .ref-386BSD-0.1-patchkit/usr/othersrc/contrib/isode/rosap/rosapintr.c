/* rosapintr.c - ROPM: invoke (interruptable) */

#ifndef	lint
static char *rcsid = "$Header: /f/osi/rosap/RCS/rosapintr.c,v 7.1 91/02/22 09:41:30 mrose Interim $";
#endif

/* 
 * $Header: /f/osi/rosap/RCS/rosapintr.c,v 7.1 91/02/22 09:41:30 mrose Interim $
 *
 *
 * $Log:	rosapintr.c,v $
 * Revision 7.1  91/02/22  09:41:30  mrose
 * Interim 6.8
 * 
 * Revision 6.0  89/03/18  23:42:25  mrose
 * Release 5.0
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

#include <stdio.h>
#include <signal.h>
#include "rosap.h"

/*  */

static int interrupted;
SFD	intrser ();

/*    RO-INVOKE.REQUEST (interruptable) */

int	RoIntrRequest (sd, op, args, invokeID, linkedID, priority, roi)
int	sd;
int	op,
    	invokeID,
       *linkedID,
	priority;
PE	args;
struct RoSAPindication *roi;
{
    int	    nfds,
	    result;
    fd_set  rfds;
    SFP    istat;

    if (RoInvokeRequest (sd, op, ROS_ASYNC, args, invokeID, linkedID, priority,
			 roi) == NOTOK)
	return NOTOK;

    interrupted = 0;
    istat = signal (SIGINT, intrser);

    for (;;) {
	nfds = 0;
	FD_ZERO (&rfds);

						/* interrupt causes EINTR */
	if (RoSelectMask (sd, &rfds, &nfds, roi) == OK)
	    (void) xselect (nfds, &rfds, NULLFD, NULLFD, NOTOK);

	if (interrupted) {
	    result = rosaplose (roi, ROS_INTERRUPTED, NULLCP, NULLCP);
	    break;
	}

	if ((result = RoWaitRequest (sd, OK, roi)) != NOTOK
	        || roi -> roi_preject.rop_reason != ROS_TIMER)
	    break;
    }

    (void) signal (SIGINT, istat);

    return result;
}

/*  */

/* ARGSUSED */

static  SFD intrser (sig)
int	sig;
{
#ifndef	BSDSIGS
    (void) signal (SIGINT, intrser);
#endif

    interrupted++;
}

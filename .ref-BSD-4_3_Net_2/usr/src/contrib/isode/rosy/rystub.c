/* rystub.c - ROSY: stubs */

#ifndef	lint
static char *rcsid = "$Header: /f/osi/rosy/RCS/rystub.c,v 7.1 91/02/22 09:42:06 mrose Interim $";
#endif

/* 
 * $Header: /f/osi/rosy/RCS/rystub.c,v 7.1 91/02/22 09:42:06 mrose Interim $
 *
 *
 * $Log:	rystub.c,v $
 * Revision 7.1  91/02/22  09:42:06  mrose
 * Interim 6.8
 * 
 * Revision 6.0  89/03/18  23:42:58  mrose
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

#include <signal.h>
#include <stdio.h>
#include "rosy.h"


#define	missingP(p) \
{ \
    if (p == NULL) \
	return rosaplose (roi, ROS_PARAMETER, NULLCP, \
			    "mandatory parameter \"%s\" missing", "p"); \
}

/*  */

static int interrupted;
SFD	intrser ();

/*    stub */

int	RyStub (sd, ryo, op, id, linked, in, rfx, efx, class, roi)
int	sd;
register struct RyOperation *ryo;
int	op,
	id,
       *linked,
    	class;
caddr_t	in;
IFP	rfx,
    	efx;
struct RoSAPindication *roi;
{
    int     firstime,
	    opclass,
	    result;
    SFP	    istat;

#ifdef	notdef			/* let RyOpInvoke check these as necessary */
    missingP (ryo);
    missingP (in);
    missingP (rfx);
    missingP (efx);
#endif
    missingP (roi);

    if ((opclass = class) == ROS_INTR) {
	interrupted = 0;
	istat = signal (SIGINT, intrser);

	opclass = ROS_ASYNC;
    }
    
    result = RyOpInvoke (sd, ryo, op, in, (caddr_t *) NULL, rfx, efx,
			 opclass, id, linked, ROS_NOPRIO, roi);
    firstime = 1;

again: ;
    switch (result) {
	case NOTOK: 
	    break;

	case OK:
	    switch (class) {
		case ROS_ASYNC:
		    break;

		case ROS_INTR:
		    if (firstime) {
			for (;;) {
			    if (!interrupted) {
				int	nfds;
				fd_set	rfds;

				nfds = 0;
				FD_ZERO (&rfds);

						/* interrupt causes EINTR */
				if (RoSelectMask (sd, &rfds, &nfds, roi) == OK)
				    (void) xselect (nfds, &rfds, NULLFD,
						    NULLFD, NOTOK);
			    }
			    if (interrupted) {
				result = rosaplose (roi, ROS_INTERRUPTED,
						    NULLCP, NULLCP);
				break;
			    }
			    if ((result = RyWait (sd, &id, (caddr_t *) NULL,
						  OK, roi)) != NOTOK
			            || roi -> roi_preject.rop_reason
					    != ROS_TIMER) {
				firstime = 0;
				goto again;
			    }
			}
			break;
		    }
		    /* else fall */
		    
		default:
		    switch (roi -> roi_type) {
		        case ROI_RESULT:
		        case ROI_ERROR:
		        case ROI_UREJECT:
			    result = OK;
			    break;

			default:
			    result = rosaplose (roi, ROS_PROTOCOL, NULLCP,
						"unknown indication type=%d",
						roi -> roi_type);
			    break;
		    }
		    break;
	    }

	case DONE:
	    break;

	default: 
	    result = rosaplose (roi, ROS_PROTOCOL, NULLCP,
				"unknown return from RyInvoke=%d", result);
	    break;
    }

    if (class == ROS_INTR)
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

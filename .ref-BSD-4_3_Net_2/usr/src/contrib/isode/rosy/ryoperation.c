/* ryoperation.c - ROSY: operations */

#ifndef	lint
static char *rcsid = "$Header: /f/osi/rosy/RCS/ryoperation.c,v 7.1 91/02/22 09:42:04 mrose Interim $";
#endif

/* 
 * $Header: /f/osi/rosy/RCS/ryoperation.c,v 7.1 91/02/22 09:42:04 mrose Interim $
 *
 *
 * $Log:	ryoperation.c,v $
 * Revision 7.1  91/02/22  09:42:04  mrose
 * Interim 6.8
 * 
 * Revision 6.0  89/03/18  23:42:56  mrose
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
#include "rosy.h"


#define	missingP(p) \
{ \
    if (p == NULL) \
	return rosaplose (roi, ROS_PARAMETER, NULLCP, \
			    "mandatory parameter \"%s\" missing", "p"); \
}

/*    OPERATION */

int	RyOperation (sd, ryo, op, in, out, response, roi)
int	sd;
register struct RyOperation *ryo;
int	op,
       *response;
caddr_t	in,
       *out;
struct RoSAPindication *roi;
{
    int     result;

#ifdef	notdef			/* let RyOpInvoke check these as necessary */
    missingP (ryo);
    missingP (in);
#endif
    missingP (out);
    missingP (response);
    missingP (roi);

    switch (result = RyOpInvoke (sd, ryo, op, in, out, NULLIFP, NULLIFP,
				 ROS_SYNC, RyGenID (sd),
				 NULLIP, ROS_NOPRIO, roi)) {
	case NOTOK: 
	    return NOTOK;

	case OK: 
	    switch (roi -> roi_type) {
		case ROI_RESULT: 
		    *response = RY_RESULT;
		    return OK;

		case ROI_ERROR:	/* XXX: hope roe -> roe_error != NOTOK */
		    {
			struct RoSAPerror  *roe = &roi -> roi_error;

			*response = roe -> roe_error;
			return OK;
		    }

		case ROI_UREJECT: 
		    {
			struct RoSAPureject *rou = &roi -> roi_ureject;

			return rosaplose (roi, rou -> rou_reason, NULLCP,
				NULLCP);
		    }

		default: 
		    return rosaplose (roi, ROS_PROTOCOL, NULLCP,
			    "unknown indication type=%d", roi -> roi_type);
	    }

	case DONE: 
	    return DONE;

	default: 
	    return rosaplose (roi, ROS_PROTOCOL, NULLCP,
		    "unknown return from RyInvoke=%d", result);
    }
}

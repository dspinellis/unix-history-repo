/* rosapureject.c - ROPM: user reject */

#ifndef	lint
static char *rcsid = "$Header: /f/osi/rosap/RCS/rosapureject.c,v 7.2 91/02/22 09:41:38 mrose Interim $";
#endif

/* 
 * $Header: /f/osi/rosap/RCS/rosapureject.c,v 7.2 91/02/22 09:41:38 mrose Interim $
 *
 * Based on an TCP-based implementation by George Michaelson of University
 * College London.
 *
 *
 * $Log:	rosapureject.c,v $
 * Revision 7.2  91/02/22  09:41:38  mrose
 * Interim 6.8
 * 
 * Revision 7.1  90/10/23  20:44:22  mrose
 * update
 * 
 * Revision 6.0  89/03/18  23:42:32  mrose
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
#include "ropkt.h"

/*    RO-U-REJECT.REQUEST */

int	RoURejectRequest (sd, invokeID, reason, priority, roi)
int	sd;
int    *invokeID,
	reason,
	priority;
struct RoSAPindication *roi;
{
    SBV	    smask;
    int     result;
    PElementID id;
    register struct assocblk   *acb;

    switch (reason) {
	case ROS_IP_DUP: 	/* Invoke Problem */
	case ROS_IP_UNRECOG: 
	case ROS_IP_MISTYPED: 
	case ROS_IP_LIMIT: 
	    id = REJECT_INVOKE;
	    reason -= REJECT_INVOKE_BASE;
	    break;

	case ROS_IP_RELEASE:
	case ROS_IP_UNLINKED:
	case ROS_IP_LINKED:
	case ROS_IP_CHILD:
	    id = REJECT_COMPLETE;
	    reason -= REJECT_INVOKE_BASE;
	    break;

	case ROS_RRP_UNRECOG: 	/* Return Result Problem */
	case ROS_RRP_UNEXP: 
	case ROS_RRP_MISTYPED: 
	    id = REJECT_RESULT;
	    reason -= REJECT_RESULT_BASE;
	    break;

	case ROS_REP_UNRECOG: 	/* Return Error Problem */
	case ROS_REP_UNEXP: 
	case ROS_REP_RECERR: 
	case ROS_REP_UNEXPERR: 
	case ROS_REP_MISTYPED: 
	    id = REJECT_ERROR;
	    reason -= REJECT_ERROR_BASE;
	    break;

	default: 
	    return rosaplose (roi, ROS_PARAMETER, NULLCP,
		    "bad value for reason parameter");
    }
    missingP (roi);

    smask = sigioblock ();

    rosapPsig (acb, sd);

    result = RoURejectRequestAux (acb, invokeID, reason, id, priority, roi);

    (void) sigiomask (smask);

    return result;
}

/*  */

int	RoURejectRequestAux (acb, invokeID, reason, id, priority, roi)
register struct assocblk   *acb;
int    *invokeID,
	reason,
	priority;
PElementID id;
struct RoSAPindication *roi;
{
    register PE pe,
		p;

    if (id == REJECT_COMPLETE)
	if (acb -> acb_flags & ACB_ACS)
	    id = REJECT_INVOKE;
	else
	    return rosaplose (roi, ROS_PARAMETER, NULLCP,
			"bad value for reason parameter");

    if (acb -> acb_ready
	    && !(acb -> acb_flags & ACB_TURN)
	    && (*acb -> acb_ready) (acb, priority, roi) == NOTOK)
	return NOTOK;

/* begin Reject APDU */
    if ((pe = pe_alloc (PE_CLASS_CONT, PE_FORM_CONS, APDU_REJECT)) == NULLPE
	    || ((acb -> acb_flags & ACB_ACS)
		    ? (p = pe, 0)
		    : set_add (pe, p = pe_alloc (PE_CLASS_UNIV, PE_FORM_CONS,
			PE_CONS_SEQ)) == NOTOK)
	    || seq_add (p, invokeID ? int2prim (*invokeID)
				  : pe_alloc (PE_CLASS_UNIV, PE_FORM_PRIM,
						PE_PRIM_NULL), -1) == NOTOK
	    || seq_add (p, num2prim ((integer) reason, PE_CLASS_CONT, id), -1)
		    == NOTOK) {
	if (pe)
	    pe_free (pe);
	freeacblk (acb);
	return rosaplose (roi, ROS_CONGEST, NULLCP, "out of memory");
    }
/* end Reject APDU */

    return (*acb -> acb_putosdu) (acb, pe, NULLPE, priority, roi);
}

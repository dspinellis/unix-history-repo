/* rosapuerror.c - ROPM: error */

#ifndef	lint
static char *rcsid = "$Header: /f/osi/rosap/RCS/rosapuerror.c,v 7.3 91/02/22 09:41:37 mrose Interim $";
#endif

/* 
 * $Header: /f/osi/rosap/RCS/rosapuerror.c,v 7.3 91/02/22 09:41:37 mrose Interim $
 *
 * Based on an TCP-based implementation by George Michaelson of University
 * College London.
 *
 *
 * $Log:	rosapuerror.c,v $
 * Revision 7.3  91/02/22  09:41:37  mrose
 * Interim 6.8
 * 
 * Revision 7.2  90/11/05  13:33:15  mrose
 * update
 * 
 * Revision 7.1  90/10/17  11:56:11  mrose
 * sync
 * 
 * Revision 6.0  89/03/18  23:42:31  mrose
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

/*    RO-ERROR.REQUEST */

int	RoErrorRequest (sd, invokeID, error, params, priority, roi)
int	sd;
int	invokeID,
	error,
	priority;
PE	params;
struct RoSAPindication *roi;
{
    SBV	    smask;
    int     result;
    register struct assocblk   *acb;

    missingP (roi);

    smask = sigioblock ();

    rosapPsig (acb, sd);

    result = RoErrorRequestAux (acb, invokeID, error, params, priority, roi);

    (void) sigiomask (smask);

    return result;
}

/*  */

static int  RoErrorRequestAux (acb, invokeID, error, params, priority, roi)
register struct assocblk   *acb;
int	invokeID,
	error,
	priority;
PE	params;
struct RoSAPindication *roi;
{
    register PE	pe,
		p;

    if ((acb -> acb_flags & ACB_INIT) && (acb -> acb_flags & ACB_ROS))
	return rosaplose (roi, ROS_OPERATION, NULLCP, "not responder");
    if (!(acb -> acb_flags & ACB_ACS)) {
	missingP (params);
    }

    if (acb -> acb_ready
	    && !(acb -> acb_flags & ACB_TURN)
	    && (*acb -> acb_ready) (acb, priority, roi) == NOTOK)
	return NOTOK;

/* begin Error APDU */
    if ((pe = pe_alloc (PE_CLASS_CONT, PE_FORM_CONS, APDU_ERROR)) == NULLPE
	    || ((acb -> acb_flags & ACB_ACS)
		    ? (p = pe, 0)
		    : set_add (pe, p = pe_alloc (PE_CLASS_UNIV, PE_FORM_CONS,
			PE_CONS_SEQ)) == NOTOK)
	    || seq_add (p, int2prim (invokeID), -1) == NOTOK
	    || seq_add (p, int2prim (error), -1) == NOTOK
	    || (params && seq_add (p, params, -1) == NOTOK)) {
	if (pe) {
	    if (params)
		    (void) pe_extract (pe, params);
	    pe_free (pe);
	}
	freeacblk (acb);
	return rosaplose (roi, ROS_CONGEST, NULLCP, "out of memory");
    }
/* end Error APDU */

    return (*acb -> acb_putosdu) (acb, pe, params, priority, roi);
}

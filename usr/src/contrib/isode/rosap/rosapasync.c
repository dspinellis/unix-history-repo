/* rosapasync.c - ROPM: set asynchronous events */

#ifndef	lint
static char *rcsid = "$Header: /f/osi/rosap/RCS/rosapasync.c,v 7.2 91/02/22 09:41:28 mrose Interim $";
#endif

/* 
 * $Header: /f/osi/rosap/RCS/rosapasync.c,v 7.2 91/02/22 09:41:28 mrose Interim $
 *
 * Based on an TCP-based implementation by George Michaelson of University
 * College London.
 *
 *
 * $Log:	rosapasync.c,v $
 * Revision 7.2  91/02/22  09:41:28  mrose
 * Interim 6.8
 * 
 * Revision 7.1  90/08/08  14:13:55  mrose
 * update
 * 
 * Revision 6.0  89/03/18  23:42:23  mrose
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

/*    define vectors for INDICATION events */

int	RoSetIndications (sd, indication, roi)
int	sd;
IFP	indication;
struct RoSAPindication *roi;
{
    SBV	    smask;
    int     result;
    register struct assocblk   *acb;

    _iosignals_set = 1;

    smask = sigioblock ();

    rosapPsig (acb, sd);

    if (acb -> acb_apdu || (acb -> acb_flags & ACB_CLOSING)) {
	(void) sigiomask (smask);
	return rosaplose (roi, ROS_WAITING, NULLCP, NULLCP);
    }

    result = (*acb -> acb_rosetindications) (acb, indication, roi);

    (void) sigiomask (smask);

    return result;
}

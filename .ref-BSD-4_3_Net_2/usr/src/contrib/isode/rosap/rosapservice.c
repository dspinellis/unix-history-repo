/* rosapservice.c - ROPM: hack loader */

#ifndef	lint
static char *rcsid = "$Header: /f/osi/rosap/RCS/rosapservice.c,v 7.1 91/02/22 09:41:36 mrose Interim $";
#endif

/* 
 * $Header: /f/osi/rosap/RCS/rosapservice.c,v 7.1 91/02/22 09:41:36 mrose Interim $
 *
 * Based on an TCP-based implementation by George Michaelson of University
 * College London.
 *
 *
 * $Log:	rosapservice.c,v $
 * Revision 7.1  91/02/22  09:41:36  mrose
 * Interim 6.8
 * 
 * Revision 6.0  89/03/18  23:42:30  mrose
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

/*    bind underlying service */

int	RoSetService (sd, bfunc, roi)
int	sd;
IFP	bfunc;
struct RoSAPindication *roi;
{
    SBV	    smask;
    int     result;
    register struct assocblk   *acb;

    missingP (bfunc);
    missingP (roi);

    smask = sigioblock ();

    if ((acb = findacblk (sd)) == NULL) {
	(void) sigiomask (smask);
	return rosaplose (roi, ROS_PARAMETER, NULLCP,
		"invalid association descriptor");
    }

    result = (*bfunc) (acb, roi);

    (void) sigiomask (smask);

    return result;
}

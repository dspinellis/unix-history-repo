/* rosapselect.c - ROPM: map descriptors */

#ifndef	lint
static char *rcsid = "$Header: /f/osi/rosap/RCS/rosapselect.c,v 7.1 91/02/22 09:41:35 mrose Interim $";
#endif

/* 
 * $Header: /f/osi/rosap/RCS/rosapselect.c,v 7.1 91/02/22 09:41:35 mrose Interim $
 *
 * Based on an TCP-based implementation by George Michaelson of University
 * College London.
 *
 *
 * $Log:	rosapselect.c,v $
 * Revision 7.1  91/02/22  09:41:35  mrose
 * Interim 6.8
 * 
 * Revision 6.0  89/03/18  23:42:29  mrose
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

/*    map association descriptors for select() */

/* ARGSUSED */

int	RoSelectMask (sd, mask, nfds, roi)
int	sd;
fd_set *mask;
int    *nfds;
struct RoSAPindication *roi;
{
    SBV	    smask;
    int     result;
    register struct assocblk   *acb;

    missingP (mask);
    missingP (nfds);
    missingP (roi);

    smask = sigioblock ();

    rosapPsig (acb, sd);

    if (acb -> acb_apdu || (acb -> acb_flags & ACB_CLOSING)) {
	(void) sigiomask (smask);
	return rosaplose (roi, ROS_WAITING, NULLCP, NULLCP);
    }

    result = (*acb -> acb_roselectmask) (acb, mask, nfds, roi);

    (void) sigiomask (smask);

    return result;
}

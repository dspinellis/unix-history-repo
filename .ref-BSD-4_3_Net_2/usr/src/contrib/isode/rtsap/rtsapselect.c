/* rtsapselect.c - RTPM: map descriptors */

#ifndef	lint
static char *rcsid = "$Header: /f/osi/rtsap/RCS/rtsapselect.c,v 7.1 91/02/22 09:42:42 mrose Interim $";
#endif

/* 
 * $Header: /f/osi/rtsap/RCS/rtsapselect.c,v 7.1 91/02/22 09:42:42 mrose Interim $
 *
 *
 * $Log:	rtsapselect.c,v $
 * Revision 7.1  91/02/22  09:42:42  mrose
 * Interim 6.8
 * 
 * Revision 6.0  89/03/18  23:43:31  mrose
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
#include "rtpkt.h"

/*    map association descriptors for select() */

int	RtSelectMask (sd, mask, nfds, rti)
int	sd;
fd_set *mask;
int    *nfds;
struct RtSAPindication *rti;
{
    SBV	    smask;
    int     result;
    register struct assocblk   *acb;

    missingP (mask);
    missingP (nfds);
    missingP (rti);

    smask = sigioblock ();

    rtsapPsig (acb, sd);

    if (acb -> acb_flags & ACB_PLEASE) {
	(void) sigiomask (smask);

	return rtsaplose (rti, RTS_WAITING, NULLCP, NULLCP);
    }

    result = (*acb -> acb_rtselectmask) (acb, mask, nfds, rti);

    (void) sigiomask (smask);

    return result;
}

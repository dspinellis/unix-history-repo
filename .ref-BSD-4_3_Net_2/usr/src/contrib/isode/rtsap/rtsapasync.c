/* rtsapasync.c - RTPM: set asynchronous events */

#ifndef	lint
static char *rcsid = "$Header: /f/osi/rtsap/RCS/rtsapasync.c,v 7.2 91/02/22 09:42:36 mrose Interim $";
#endif

/* 
 * $Header: /f/osi/rtsap/RCS/rtsapasync.c,v 7.2 91/02/22 09:42:36 mrose Interim $
 *
 *
 * $Log:	rtsapasync.c,v $
 * Revision 7.2  91/02/22  09:42:36  mrose
 * Interim 6.8
 * 
 * Revision 7.1  90/08/08  14:13:59  mrose
 * update
 * 
 * Revision 6.0  89/03/18  23:43:26  mrose
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

/*    define vectors for INDICATION events */

int	RtSetIndications (sd, indication, rti)
int	sd;
IFP	indication;
struct RtSAPindication *rti;
{
    SBV	    smask;
    int     result;
    register struct assocblk   *acb;

    _iosignals_set = 1;

    smask = sigioblock ();

    rtsapPsig (acb, sd);

    if (acb -> acb_flags & ACB_PLEASE) {
	(void) sigiomask (smask);

	return rtsaplose (rti, RTS_WAITING, NULLCP, NULLCP);
    }

    result = (*acb -> acb_rtsetindications) (acb, indication, rti);

    (void) sigiomask (smask);

    return result;
}

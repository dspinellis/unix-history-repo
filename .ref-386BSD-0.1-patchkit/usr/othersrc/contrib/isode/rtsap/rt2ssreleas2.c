/* rt2ssreleas2.c - RTPM: respond to release */

#ifndef	lint
static char *rcsid = "$Header: /f/osi/rtsap/RCS/rt2ssreleas2.c,v 7.1 91/02/22 09:42:32 mrose Interim $";
#endif

/* 
 * $Header: /f/osi/rtsap/RCS/rt2ssreleas2.c,v 7.1 91/02/22 09:42:32 mrose Interim $
 *
 *
 * $Log:	rt2ssreleas2.c,v $
 * Revision 7.1  91/02/22  09:42:32  mrose
 * Interim 6.8
 * 
 * Revision 6.0  89/03/18  23:43:22  mrose
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

/*    RT-END.RESPONSE (X.410 CLOSE.RESPONSE) */

int	RtEndResponse (sd, rti)
int	sd;
struct RtSAPindication *rti;
{
    SBV	    smask;
    int     result;
    register struct assocblk   *acb;

    missingP (rti);

    smask = sigioblock ();

    rtsapFsig (acb, sd);

    result = RtEndResponseAux (acb, rti);

    (void) sigiomask (smask);

    return result;

}

/*  */

static int  RtEndResponseAux (acb, rti)
register struct assocblk   *acb;
struct RtSAPindication *rti;
{
    int     result;
    struct SSAPindication   sis;
    register struct SSAPindication *si = &sis;
    register struct SSAPabort  *sa = &si -> si_abort;

    if (acb -> acb_flags & ACB_ACS)
	return rtsaplose (rti, RTS_OPERATION, NULLCP,
		    "not an association descriptor for RTS");

    if (SRelResponse (acb -> acb_fd, SC_ACCEPT, NULLCP, 0, si) == NOTOK)
	result = ss2rtslose (acb, rti, "SRelResponse", sa);
    else {
	acb -> acb_fd = NOTOK;
	result = OK;
    }

    acb -> acb_flags &= ~ACB_STICKY;
    freeacblk (acb);

    return result;
}

/* rt2psreleas2.c - RTPM: respond to release */

#ifndef	lint
static char *rcsid = "$Header: /f/osi/rtsap/RCS/rt2psreleas2.c,v 7.1 91/02/22 09:42:21 mrose Interim $";
#endif

/* 
 * $Header: /f/osi/rtsap/RCS/rt2psreleas2.c,v 7.1 91/02/22 09:42:21 mrose Interim $
 *
 *
 * $Log:	rt2psreleas2.c,v $
 * Revision 7.1  91/02/22  09:42:21  mrose
 * Interim 6.8
 * 
 * Revision 6.0  89/03/18  23:43:12  mrose
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

/*    RT-CLOSE.RESPONSE */

int	RtCloseResponse (sd, reason, data, rti)
int	sd,
	reason;
PE	data;
struct RtSAPindication *rti;
{
    SBV	    smask;
    int	    result;
    register struct assocblk *acb;

    missingP (rti);

    smask = sigioblock ();

    rtsapFsig (acb, sd);

    result = RtCloseResponseAux (acb, reason, data, rti);

    (void) sigiomask (smask);

    return result;
}

/*  */

static int  RtCloseResponseAux (acb, reason, data, rti)
register struct assocblk *acb;
int	reason;
PE	data;
register struct RtSAPindication *rti;
{
    int	    result;
    struct AcSAPindication acis;
    register struct AcSAPindication *aci = &acis;
    register struct AcSAPabort *aca = &aci -> aci_abort;

    if (!(acb -> acb_flags & ACB_ACS))
	return rtsaplose (rti, RTS_OPERATION, NULLCP,
		    "not an association descriptor for RTS");

    if (data)
	data -> pe_context = acb -> acb_rtsid;

    acb -> acb_flags &= ~ACB_STICKY;
    if (AcRelResponse (acb -> acb_fd, ACS_ACCEPT, reason, &data, data ? 1 : 0,
	    aci) == NOTOK)
	result = acs2rtslose (acb, rti, "AcRelResponse", aca);
    else
	result = OK;

    return result;
}

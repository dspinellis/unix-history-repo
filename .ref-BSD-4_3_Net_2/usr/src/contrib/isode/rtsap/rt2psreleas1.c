/* rt2psreleas1.c - RTPM: initiate release */

#ifndef	lint
static char *rcsid = "$Header: /f/osi/rtsap/RCS/rt2psreleas1.c,v 7.2 91/02/22 09:42:20 mrose Interim $";
#endif

/* 
 * $Header: /f/osi/rtsap/RCS/rt2psreleas1.c,v 7.2 91/02/22 09:42:20 mrose Interim $
 *
 *
 * $Log:	rt2psreleas1.c,v $
 * Revision 7.2  91/02/22  09:42:20  mrose
 * Interim 6.8
 * 
 * Revision 7.1  90/07/01  21:06:55  mrose
 * pepsy
 * 
 * Revision 6.0  89/03/18  23:43:11  mrose
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

/*    RT-CLOSE.REQUEST */

int	RtCloseRequest (sd, reason, data, acr, rti)
int	sd,
	reason;
PE	data;
struct AcSAPrelease *acr;
struct RtSAPindication *rti;
{
    SBV	    smask;
    int	    result;
    register struct assocblk *acb;

    missingP (acr);
    missingP (rti);

    smask = sigioblock ();

    rtsapPsig (acb, sd);

    result = RtCloseRequestAux (acb, reason, data, acr, rti);

    (void) sigiomask (smask);

    return result;
}

/*  */

static int  RtCloseRequestAux (acb, reason, data, acr, rti)
register struct assocblk *acb;
int	reason;
PE	data;
struct AcSAPrelease *acr;
register struct RtSAPindication *rti;
{
    int	    result;
    struct AcSAPindication acis;
    register struct AcSAPindication *aci = &acis;
    register struct AcSAPabort *aca = &aci -> aci_abort;

    if (!(acb -> acb_flags & ACB_ACS))
	return rtsaplose (rti, RTS_OPERATION, NULLCP,
		    "not an association descriptor for RTS");
    if (!(acb -> acb_flags & ACB_INIT) && (acb -> acb_flags & ACB_TWA))
	return rtsaplose (rti, RTS_OPERATION, NULLCP, "not initiator");
    if (!(acb -> acb_flags & ACB_TURN))
	return rtsaplose (rti, RTS_OPERATION, NULLCP, "turn not owned by you");
    if (acb -> acb_flags & ACB_ACT)
	return rtsaplose (rti, RTS_OPERATION, NULLCP, "transfer in progress");
    if (acb -> acb_flags & ACB_PLEASE)
	return rtsaplose (rti, RTS_WAITING, NULLCP, NULLCP);

    if (data)
	data -> pe_context = acb -> acb_rtsid;

    acb -> acb_flags &= ~ACB_STICKY;
    if (AcRelRequest (acb -> acb_fd, reason, &data, data ? 1 : 0, NOTOK, acr, aci)
	    == NOTOK) {
	if (aca -> aca_source == ACA_USER)
	    result = acs2rtsabort (acb, aca, rti);
	else
	    result = acs2rtslose (acb, rti, "AcRelRequest", aca);
    }
    else
	if (!acr -> acr_affirmative)
	    result = rtpktlose (acb, rti, RTS_PROTOCOL, NULLCP,
			    "other side refused to release association");
	else
	    result = OK;

    return result;
}

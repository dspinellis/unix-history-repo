/* rtsaptrans.c - RTPM: transfer */

#ifndef	lint
static char *rcsid = "$Header: /f/osi/rtsap/RCS/rtsaptrans.c,v 7.1 91/02/22 09:42:43 mrose Interim $";
#endif

/* 
 * $Header: /f/osi/rtsap/RCS/rtsaptrans.c,v 7.1 91/02/22 09:42:43 mrose Interim $
 *
 *
 * $Log:	rtsaptrans.c,v $
 * Revision 7.1  91/02/22  09:42:43  mrose
 * Interim 6.8
 * 
 * Revision 6.0  89/03/18  23:43:32  mrose
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

/*    RT-TRANSFER.REQUEST */

int	RtTransferRequest (sd, data, secs, rti)
int	sd;
PE	data;
int	secs;
struct RtSAPindication *rti;
{
    SBV	    smask;
    int     result;
    register struct assocblk   *acb;

    missingP (rti);

    smask = sigioblock ();

    rtsapPsig (acb, sd);

    if (data == NULLPE && acb -> acb_downtrans == NULLIFP) {
	(void) sigiomask (smask);
	return rtsaplose (rti, RTS_PARAMETER, NULLCP,
			  "mandatory parameter \"data\" missing");
    }

    result = (*acb -> acb_transferequest)  (acb, data, secs, rti);

    (void) sigiomask (smask);

    return result;
}

/* rtsapdtrans.c - RTPM: set downtrans upcall */

#ifndef	lint
static char *rcsid = "$Header: /f/osi/rtsap/RCS/rtsapdtrans.c,v 7.1 91/02/22 09:42:37 mrose Interim $";
#endif

/* 
 * $Header: /f/osi/rtsap/RCS/rtsapdtrans.c,v 7.1 91/02/22 09:42:37 mrose Interim $
 *
 *
 * $Log:	rtsapdtrans.c,v $
 * Revision 7.1  91/02/22  09:42:37  mrose
 * Interim 6.8
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

/*    set downtrans upcall */

int	RtSetDownTrans (sd, fnx, rti)
int	sd;
IFP	fnx;
struct RtSAPindication *rti;
{
    SBV	    smask;
    register struct assocblk   *acb;

    missingP (rti);

    smask = sigioblock ();

    rtsapPsig (acb, sd);

    acb -> acb_downtrans = fnx;

    (void) sigiomask (smask);

    return OK;
}

/* rtsaputrans.c - RTPM: set uptrans upcall */

#ifndef	lint
static char *rcsid = "$Header: /f/osi/rtsap/RCS/rtsaputrans.c,v 7.1 91/02/22 09:42:44 mrose Interim $";
#endif

/* 
 * $Header: /f/osi/rtsap/RCS/rtsaputrans.c,v 7.1 91/02/22 09:42:44 mrose Interim $
 *
 *
 * $Log:	rtsaputrans.c,v $
 * Revision 7.1  91/02/22  09:42:44  mrose
 * Interim 6.8
 * 
 * Revision 6.0  89/03/18  23:43:33  mrose
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

/*    set uptrans upcall */

int	RtSetUpTrans (sd, fnx, rti)
int	sd;
IFP	fnx;
struct RtSAPindication *rti;
{
    SBV	    smask;
    register struct assocblk   *acb;

    missingP (rti);

    smask = sigioblock ();

    rtsapPsig (acb, sd);

    acb -> acb_uptrans = fnx;

    (void) sigiomask (smask);

    return OK;
}

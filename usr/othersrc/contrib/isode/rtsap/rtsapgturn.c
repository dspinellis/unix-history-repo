/* rtsapgturn.c - RTPM: give turn */

#ifndef	lint
static char *rcsid = "$Header: /f/osi/rtsap/RCS/rtsapgturn.c,v 7.1 91/02/22 09:42:39 mrose Interim $";
#endif

/* 
 * $Header: /f/osi/rtsap/RCS/rtsapgturn.c,v 7.1 91/02/22 09:42:39 mrose Interim $
 *
 *
 * $Log:	rtsapgturn.c,v $
 * Revision 7.1  91/02/22  09:42:39  mrose
 * Interim 6.8
 * 
 * Revision 6.0  89/03/18  23:43:28  mrose
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

/*    RT-TURN-GIVE.REQUEST */

int	RtGTurnRequest (sd, rti)
int	sd;
struct RtSAPindication *rti;
{
    SBV	    smask;
    int     result;
    register struct assocblk   *acb;

    missingP (rti);

    smask = sigioblock ();

    rtsapPsig (acb, sd);

    result = (*acb -> acb_gturnrequest) (acb, rti);

    (void) sigiomask (smask);

    return result;
}

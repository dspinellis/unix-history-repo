/* rtsappturn.c - RTPM: turn please */

#ifndef	lint
static char *rcsid = "$Header: /f/osi/rtsap/RCS/rtsappturn.c,v 7.1 91/02/22 09:42:41 mrose Interim $";
#endif

/* 
 * $Header: /f/osi/rtsap/RCS/rtsappturn.c,v 7.1 91/02/22 09:42:41 mrose Interim $
 *
 *
 * $Log:	rtsappturn.c,v $
 * Revision 7.1  91/02/22  09:42:41  mrose
 * Interim 6.8
 * 
 * Revision 6.0  89/03/18  23:43:30  mrose
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

/*    RT-TURN-PLEASE.REQUEST */

int	RtPTurnRequest (sd, priority, rti)
int	sd;
int	priority;
struct RtSAPindication *rti;
{
    SBV	    smask;
    int     result;
    register struct assocblk   *acb;

    missingP (rti);

    smask = sigioblock ();

    rtsapPsig (acb, sd);

    result = (*acb -> acb_pturnrequest) (acb, priority, rti);

    (void) sigiomask (smask);

    return result;
}

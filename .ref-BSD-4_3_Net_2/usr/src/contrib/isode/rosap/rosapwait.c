/* rosapwait.c - ROPM: wait for an indication */

#ifndef	lint
static char *rcsid = "$Header: /f/osi/rosap/RCS/rosapwait.c,v 7.1 91/02/22 09:41:39 mrose Interim $";
#endif

/* 
 * $Header: /f/osi/rosap/RCS/rosapwait.c,v 7.1 91/02/22 09:41:39 mrose Interim $
 *
 * Based on an TCP-based implementation by George Michaelson of University
 * College London.
 *
 *
 * $Log:	rosapwait.c,v $
 * Revision 7.1  91/02/22  09:41:39  mrose
 * Interim 6.8
 * 
 * Revision 6.0  89/03/18  23:42:33  mrose
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
#include "ropkt.h"

/*    RO-WAIT.REQUEST (pseudo) */

int	RoWaitRequest (sd, secs, roi)
int	sd;
int	secs;
struct RoSAPindication *roi;
{
    SBV	    smask;
    int     result;
    register struct assocblk   *acb;

    missingP (roi);

    smask = sigioblock ();

    rosapXsig (acb, sd);

    result =  (*acb -> acb_rowaitrequest) (acb, NULLIP, secs, roi);

    (void) sigiomask (smask);

    return result;
}

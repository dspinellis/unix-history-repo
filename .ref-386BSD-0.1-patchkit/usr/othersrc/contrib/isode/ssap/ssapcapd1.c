/* ssapcapd1.c - SPM: write capability data */

#ifndef	lint
static char *rcsid = "$Header: /f/osi/ssap/RCS/ssapcapd1.c,v 7.1 91/02/22 09:45:43 mrose Interim $";
#endif

/* 
 * $Header: /f/osi/ssap/RCS/ssapcapd1.c,v 7.1 91/02/22 09:45:43 mrose Interim $
 *
 *
 * $Log:	ssapcapd1.c,v $
 * Revision 7.1  91/02/22  09:45:43  mrose
 * Interim 6.8
 * 
 * Revision 7.0  89/11/23  22:25:23  mrose
 * Release 6.0
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
#include "spkt.h"

/*    S-CAPABILITY-DATA.REQUEST */

int	SCapdRequest (sd, data, cc, si)
int	sd;
char   *data;
int	cc;
struct SSAPindication *si;
{
    SBV	    smask;
    int     result;
    register struct ssapblk *sb;

    missingP (si);

    smask = sigioblock ();

    ssapPsig (sb, sd);
    toomuchP (sb, data, cc, SX_CDSIZE, "capability");

    result = SCapdRequestAux (sb, data, cc, si);

    (void) sigiomask (smask);

    return result;
}

/*  */

static  int SCapdRequestAux (sb, data, cc, si)
register struct ssapblk *sb;
char   *data;
int	cc;
struct SSAPindication *si;
{
    int     result;

    if (!(sb -> sb_requirements & SR_CAPABILITY))
	return ssaplose (si, SC_OPERATION, NULLCP,
		"capability data exchange service unavailable");
    if (SDoActivityAux (sb, si, 1, 0) == NOTOK)
	return NOTOK;
    if (sb -> sb_flags & SB_CD)
	return ssaplose (si, SC_OPERATION, NULLCP,
		"capability data request in progress");
    sb -> sb_flags |= SB_CD;

    if ((result = SWriteRequestAux (sb, SPDU_CD, data, cc, 0, 0L, 0, NULLSD,
		NULLSD, NULLSR, si)) == NOTOK)
	freesblk (sb);

    return result;
}

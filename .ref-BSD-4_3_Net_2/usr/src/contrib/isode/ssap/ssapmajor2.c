/* ssapmajor2.c - SPM: respond to majorsyncs */

#ifndef	lint
static char *rcsid = "$Header: /f/osi/ssap/RCS/ssapmajor2.c,v 7.1 91/02/22 09:45:53 mrose Interim $";
#endif

/* 
 * $Header: /f/osi/ssap/RCS/ssapmajor2.c,v 7.1 91/02/22 09:45:53 mrose Interim $
 *
 *
 * $Log:	ssapmajor2.c,v $
 * Revision 7.1  91/02/22  09:45:53  mrose
 * Interim 6.8
 * 
 * Revision 7.0  89/11/23  22:25:31  mrose
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

/*    S-MAJOR-SYNC.RESPONSE */

int	SMajSyncResponse (sd, data, cc, si)
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
    toomuchP (sb, data, cc, SN_SIZE, "majorsync");

    result = SMajSyncResponseAux (sb, data, cc, si);

    (void) sigiomask (smask);

    return result;
}

/*  */

int	SMajSyncResponseAux (sb, data, cc, si)
register struct ssapblk *sb;
char   *data;
int	cc;
register struct SSAPindication *si;
{
    int     result;

    if (!(sb -> sb_requirements & SR_MAJORSYNC)
	    && !(sb -> sb_requirements & SR_ACTIVITY)
	    && !(sb -> sb_flags & SB_Vact))
	return ssaplose (si, SC_OPERATION, NULLCP,
		"major synchronize service unavailable");
    if (!(sb -> sb_flags & SB_MAA))
	return ssaplose (si, SC_OPERATION, NULLCP,
		"no majorsync in progress");

    if ((result = SWriteRequestAux (sb, SPDU_MAA, data, cc, 0,
	    sb -> sb_V_M - 1, 0, NULLSD, NULLSD, NULLSR, si)) == NOTOK)
	freesblk (sb);
    else {
	sb -> sb_V_A = sb -> sb_V_R = sb -> sb_V_M;
	if (sb -> sb_requirements & SR_ACTIVITY)
	    if (sb -> sb_flags & SB_Vnextact)
		sb -> sb_flags |= SB_Vact;
	    else
		sb -> sb_flags &= ~SB_Vact;

	sb -> sb_flags &= ~(SB_MAA | SB_AE);
    }

    return result;
}

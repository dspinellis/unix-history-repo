/* ssapminor2.c - SPM: respond to minorsyncs */

#ifndef	lint
static char *rcsid = "$Header: /f/osi/ssap/RCS/ssapminor2.c,v 7.1 91/02/22 09:45:55 mrose Interim $";
#endif

/* 
 * $Header: /f/osi/ssap/RCS/ssapminor2.c,v 7.1 91/02/22 09:45:55 mrose Interim $
 *
 *
 * $Log:	ssapminor2.c,v $
 * Revision 7.1  91/02/22  09:45:55  mrose
 * Interim 6.8
 * 
 * Revision 7.0  89/11/23  22:25:33  mrose
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

/*    S-MINOR-SYNC.RESPONSE */

int	SMinSyncResponse (sd, ssn, data, cc, si)
int	sd;
long	ssn;
char   *data;
int	cc;
struct SSAPindication *si;
{
    SBV	    smask;
    int     result;
    register struct ssapblk *sb;

    if (SERIAL_MIN > ssn || ssn > SERIAL_MAX)
	return ssaplose (si, SC_PARAMETER, NULLCP, "invalid serial number");
    missingP (si);

    smask = sigioblock ();

    ssapPsig (sb, sd);
    toomuchP (sb, data, cc, SN_SIZE, "minorsync");

    result = SMinSyncResponseAux (sb, ssn, data, cc, si);

    (void) sigiomask (smask);

    return result;
}

/*  */

static int  SMinSyncResponseAux (sb, ssn, data, cc, si)
register struct ssapblk *sb;
long	ssn;
char   *data;
int	cc;
register struct SSAPindication *si;
{
    int     result;

    if (!(sb -> sb_requirements & SR_MINORSYNC))
	return ssaplose (si, SC_OPERATION, NULLCP,
		"minor synchronize service unavailable");
    if (!(sb -> sb_flags & SB_Vsc))
	return ssaplose (si, SC_OPERATION, NULLCP,
		"no minorsync in progress");
    if (ssn < sb -> sb_V_A)
	return ssaplose (si, SC_OPERATION, NULLCP,
		"bad choice for minor ssn, should be >= %ld", sb -> sb_V_A);

    if ((result = SWriteRequestAux (sb, SPDU_MIA, data, cc, 0, ssn, 0, NULLSD,
		NULLSD, NULLSR, si)) == NOTOK)
	freesblk (sb);
    else
	sb -> sb_V_A = ssn + 1;

    return result;
}

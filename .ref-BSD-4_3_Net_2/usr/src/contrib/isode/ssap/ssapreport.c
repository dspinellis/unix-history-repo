/* ssapreport.c - SPM: exception reports */

#ifndef	lint
static char *rcsid = "$Header: /f/osi/ssap/RCS/ssapreport.c,v 7.1 91/02/22 09:45:58 mrose Interim $";
#endif

/* 
 * $Header: /f/osi/ssap/RCS/ssapreport.c,v 7.1 91/02/22 09:45:58 mrose Interim $
 *
 *
 * $Log:	ssapreport.c,v $
 * Revision 7.1  91/02/22  09:45:58  mrose
 * Interim 6.8
 * 
 * Revision 7.0  89/11/23  22:25:37  mrose
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

/*    S-U-EXCEPTION-REPORT.REQUEST */

int	SUReportRequest (sd, reason, data, cc, si)
int	sd;
int	reason;
char   *data;
int	cc;
struct SSAPindication *si;
{
    SBV	    smask;
    int     result;
    register struct ssapblk *sb;

    if (!(SP_OK (reason)))
	return ssaplose (si, SC_PARAMETER, NULLCP, "invalid reason");
    missingP (si);

    smask = sigioblock ();

    ssapPsig (sb, sd);
    toomuchP (sb, data, cc, SP_SIZE, "report");

    result = SUReportRequestAux (sb, reason, data, cc, si);

    (void) sigiomask (smask);

    return result;
}

/*  */

static int  SUReportRequestAux (sb, reason, data, cc, si)
register struct ssapblk *sb;
int	reason;
char   *data;
int	cc;
register struct SSAPindication *si;
{
    int	    result;
    
    if (!(sb -> sb_requirements & SR_EXCEPTIONS))
	return ssaplose (si, SC_OPERATION, NULLCP,
		"exceptions service unavailable");
    if (!(sb -> sb_requirements & SR_DAT_EXISTS))
	return ssaplose (si, SC_OPERATION, NULLCP,
		"data token not available");
    if (sb -> sb_owned & ST_DAT_TOKEN)
	return ssaplose (si, SC_OPERATION, NULLCP,
		"data token owned by you");
    if ((sb -> sb_requirements & SR_ACTIVITY)
	    && !(sb -> sb_flags & SB_Vact))
	return ssaplose (si, SC_OPERATION, NULLCP,
		"no activity in progress");

    if ((result = SWriteRequestAux (sb, SPDU_ED, data, cc, reason, 0L, 0,
	    NULLSD, NULLSD, NULLSR, si)) == NOTOK)
	freesblk (sb);
    else
	sb -> sb_flags |= SB_ED;

    return result;
}

/* ssaprelease2.c - SPM: respond to release */

#ifndef	lint
static char *rcsid = "$Header: /f/osi/ssap/RCS/ssaprelease2.c,v 7.1 91/02/22 09:45:57 mrose Interim $";
#endif

/* 
 * $Header: /f/osi/ssap/RCS/ssaprelease2.c,v 7.1 91/02/22 09:45:57 mrose Interim $
 *
 *
 * $Log:	ssaprelease2.c,v $
 * Revision 7.1  91/02/22  09:45:57  mrose
 * Interim 6.8
 * 
 * Revision 7.0  89/11/23  22:25:36  mrose
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

/*    S-RELEASE.RESPONSE */

int	SRelResponse (sd, status, data, cc, si)
int	sd;
int	status,
	cc;
char   *data;
struct SSAPindication *si;
{
    SBV	    smask;
    int     result;
    register struct ssapblk *sb;

    missingP (si);

    smask = sigioblock ();

    ssapFsig (sb, sd);
    toomuchP (sb, data, cc, SR_SIZE, "release");

    result = SRelResponseAux (sb, status, data, cc, si);

    (void) sigiomask (smask);

    return result;
}

/*    S-RELEASE.RESPONSE */

static int  SRelResponseAux (sb, status, data, cc, si)
register struct ssapblk *sb;
int	status,
	cc;
char   *data;
struct SSAPindication *si;
{
    int     code,
            result;
    register struct ssapkt *s;

    switch (status) {
	case SC_ACCEPT: 
	    code = SPDU_DN;
	    break;

	case SC_REJECTED:
	    if (!(sb -> sb_requirements & SR_NEGOTIATED))
		return ssaplose (si, SC_OPERATION, NULLCP,
			"negotiated release service unavailable");
	    if (!(sb -> sb_requirements & SR_RLS_EXISTS))
		return ssaplose (si, SC_OPERATION, NULLCP,
			    "release token unavailable");
	    if (sb -> sb_owned & ST_RLS_TOKEN)
		return ssaplose (si, SC_OPERATION, NULLCP,
			    "release token owned by you");

	    code = SPDU_NF;
	    break;

	default: 
	    return ssaplose (si, SC_PARAMETER, NULLCP,
			"invalid value for status parameter");
    }

    if ((s = newspkt (code)) == NULL)
	return ssaplose (si, SC_CONGEST, NULLCP, "out of memory");

    if (cc > 0) {
	s -> s_mask |= SMASK_UDATA_PGI;
	s -> s_udata = data, s -> s_ulen = cc;
    }
    else
	s -> s_udata = NULL, s -> s_ulen = 0;
    result = spkt2sd (s, sb -> sb_fd, 0, si);
    s -> s_mask &= ~SMASK_UDATA_PGI;
    s -> s_udata = NULL, s -> s_ulen = 0;

    freespkt (s);
    if (result == NOTOK || code == SPDU_DN)
	freesblk (sb);
    else
	sb -> sb_flags &= ~SB_FINN;

    return result;
}

/* ssapabort.c - SPM: user abort */

#ifndef	lint
static char *rcsid = "$Header: /f/osi/ssap/RCS/ssapabort.c,v 7.2 91/02/22 09:45:39 mrose Interim $";
#endif

/* 
 * $Header: /f/osi/ssap/RCS/ssapabort.c,v 7.2 91/02/22 09:45:39 mrose Interim $
 *
 *
 * $Log:	ssapabort.c,v $
 * Revision 7.2  91/02/22  09:45:39  mrose
 * Interim 6.8
 * 
 * Revision 7.1  90/11/21  11:31:41  mrose
 * sun
 * 
 * Revision 7.0  89/11/23  22:25:20  mrose
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
#include "tailor.h"

/*    S-U-ABORT.REQUEST */

int	SUAbortRequest (sd, data, cc, si)
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

    if ((sb = findsblk (sd)) == NULL) {
	(void) sigiomask (smask);
	return ssaplose (si, SC_PARAMETER, NULLCP, "invalid session descriptor");
    }
    toomuchP (sb, data, cc, SA_SIZE, "abort");

    result = SUAbortRequestAux (sb, data, cc, si);

    (void) sigiomask (smask);

    return result;
}

/*  */

static int  SUAbortRequestAux (sb, data, cc, si)
register struct ssapblk *sb;
char   *data;
int	cc;
struct SSAPindication *si;
{
    int     result;
    register struct ssapkt *s;
    struct TSAPdata txs;
    register struct TSAPdata   *tx = &txs;
    struct TSAPdisconnect   tds;
    register struct TSAPdisconnect *td = &tds;

    sb -> sb_flags &= ~(SB_ED | SB_EDACK | SB_ERACK);

    if ((sb -> sb_flags & SB_EXPD)
	    && sb -> sb_version >= SB_VRSN2
	    && cc > 9) {
	register struct ssapkt *p;

	if (p = newspkt (SPDU_PR)) {
	    p -> s_mask |= SMASK_PR_TYPE;
	    p -> s_pr_type = PR_AB;
	    result = spkt2sd (p, sb -> sb_fd, 1, si);
	    freespkt (p);
	    if (result == NOTOK)
		goto out1;
	}
    }
	
    if ((s = newspkt (SPDU_AB)) == NULL) {
	result = ssaplose (si, SC_CONGEST, NULLCP, "out of memory");
	goto out1;
    }

    s -> s_mask |= SMASK_SPDU_AB | SMASK_AB_DISC;
    s -> s_ab_disconnect = AB_DISC_RELEASE | AB_DISC_USER;

    if (cc > 0) {
	s -> s_mask |= SMASK_UDATA_PGI;
	s -> s_udata = data, s -> s_ulen = cc;
    }
    else
	s -> s_udata = NULL, s -> s_ulen = 0;
    result = spkt2sd (s, sb -> sb_fd, sb -> sb_flags & SB_EXPD ? 1 : 0, si);
    s -> s_mask &= ~SMASK_UDATA_PGI;
    s -> s_udata = NULL, s -> s_ulen = 0;

    freespkt (s);
    if (result == NOTOK)
	goto out1;

    if (ses_ab_timer >= 0)
	switch (TReadRequest (sb -> sb_fd, tx, ses_ab_timer, td)) {
	    case OK:
	    default: 		/* should be an ABORT ACCEPT, but who cares? */
	        TXFREE (tx);
		break;

	    case NOTOK:
		if (td -> td_reason != DR_TIMER)
		    sb -> sb_fd = NOTOK;
		break;
	}
    result = OK;

out1: ;
    freesblk (sb);
    
    return result;
}

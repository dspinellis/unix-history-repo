/* acsapreleas3.c - ACPM: interpret release */

#ifndef	lint
static char *rcsid = "$Header: /f/osi/acsap/RCS/acsapreleas3.c,v 7.2 91/02/22 09:14:15 mrose Interim $";
#endif

/* 
 * $Header: /f/osi/acsap/RCS/acsapreleas3.c,v 7.2 91/02/22 09:14:15 mrose Interim $
 *
 *
 * $Log:	acsapreleas3.c,v $
 * Revision 7.2  91/02/22  09:14:15  mrose
 * Interim 6.8
 * 
 * Revision 7.1  90/07/01  21:02:06  mrose
 * pepsy
 * 
 * Revision 7.0  89/11/23  21:21:56  mrose
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
#include "ACS-types.h"
#define	ACSE
#include "acpkt.h"
#ifdef	DEBUG
#include "tailor.h"
#endif

/*    handle P-RELEASE.INDICATION */

int	AcFINISHser (sd, pf, aci)
int	sd;
register struct PSAPfinish *pf;
register struct AcSAPindication *aci;
{
    SBV	    smask;
    int	    result;
    PE	    pe;
    register struct assocblk *acb;
    register struct AcSAPfinish *acf;
    struct type_ACS_ACSE__apdu *pdu;
    register struct type_ACS_RLRQ__apdu *rlrq;

    missingP (pf);
    missingP (aci);

    smask = sigioblock ();
    
    acsapPsig (acb, sd);

    bzero ((char *) aci, sizeof *aci);
    aci -> aci_type = ACI_FINISH;
    acf = &aci -> aci_finish;
    
    pdu = NULL;

    if (pf -> pf_ninfo < 1) {
	result = acsaplose (aci, ACS_PROTOCOL, NULLCP,
			    "no user-data on P-RELEASE");
	goto out;
    }

    result = decode_ACS_ACSE__apdu (pe = pf -> pf_info[0], 1, NULLIP, NULLVP,
				    &pdu);

#ifdef	DEBUG
    if (result == OK && (acsap_log -> ll_events & LLOG_PDUS))
	pvpdu (acsap_log, print_ACS_ACSE__apdu_P, pe, "ACSE-apdu", 1);
#endif

    pe_free (pe);
    pe = pf -> pf_info[0] = NULLPE;
    
    if (result == NOTOK) {
	result = acsaplose (aci, ACS_PROTOCOL, NULLCP, "%s", PY_pepy);
	goto out;
    }

    if (pdu -> offset != type_ACS_ACSE__apdu_rlrq) {
	result = acsaplose (aci, ACS_PROTOCOL, NULLCP,
			    "unexpected PDU %d on P-RELEASE", pdu -> offset);
	goto out;
    }

    rlrq = pdu -> un.rlrq;
    if (rlrq -> optionals & opt_ACS_RLRQ__apdu_reason)
	acf -> acf_reason = rlrq -> reason;
    else
	acf -> acf_reason = int_ACS_reason_normal;
    result = apdu2info (acb, aci, rlrq -> user__information, acf -> acf_info,
		   &acf -> acf_ninfo);

out: ;
    if (result == NOTOK)
	freeacblk (acb);
    else
	acb -> acb_flags |= ACB_FINN;

    PFFREE (pf);
    if (pdu)
	free_ACS_ACSE__apdu (pdu);

    (void) sigiomask (smask);

    return result;
}

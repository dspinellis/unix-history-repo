/* acsapreleas1.c - ACPM: initiate release */

#ifndef	lint
static char *rcsid = "$Header: /f/osi/acsap/RCS/acsapreleas1.c,v 7.2 91/02/22 09:14:12 mrose Interim $";
#endif

/* 
 * $Header: /f/osi/acsap/RCS/acsapreleas1.c,v 7.2 91/02/22 09:14:12 mrose Interim $
 *
 *
 * $Log:	acsapreleas1.c,v $
 * Revision 7.2  91/02/22  09:14:12  mrose
 * Interim 6.8
 * 
 * Revision 7.1  90/07/01  21:02:02  mrose
 * pepsy
 * 
 * Revision 7.0  89/11/23  21:21:54  mrose
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
#include "tailor.h"

/*    A-RELEASE.REQUEST */

int	AcRelRequest (sd, reason, data, ndata, secs, acr, aci)
int	sd;
int	reason;
PE     *data;
int	ndata;
int	secs;
struct AcSAPrelease *acr;
struct AcSAPindication *aci;
{
    SBV	    smask;
    int	    result;
    register struct assocblk *acb;
    register struct type_ACS_RLRQ__apdu *rlrq;

    switch (reason) {
	case ACF_NORMAL: 
	case ACF_URGENT: 
	case ACF_USERDEFINED: 
	    break;

	default: 
	    return acsaplose (aci, ACS_PARAMETER, NULLCP,
		    "invalid value for reason parameter");
    }
    toomuchP (data, ndata, NACDATA, "release");
    if (data) {	    /* XXX: probably should have a more intensive check... */
	register int    i;
	register PE    *pep;

	for (pep = data, i = ndata; i > 0; pep++, i--)
	    if ((*pep) -> pe_context == PE_DFLT_CTX)
		return acsaplose (aci, ACS_PARAMETER, NULLCP,
			"default context not allowed for user-data at slot %d",
				  pep - data);
    }
    missingP (acr);
    missingP (aci);

    smask = sigioblock ();

    acsapPsig (acb, sd);

    if ((rlrq = (struct type_ACS_RLRQ__apdu *) calloc (1, sizeof *rlrq))
		== NULL) {
	(void) acsaplose (aci, ACS_CONGEST, NULLCP, "out of memory");
	goto out2;
    }
    rlrq -> optionals |= opt_ACS_RLRQ__apdu_reason;
    rlrq -> reason = reason;
    if (data
	    && ndata > 0
	    && (rlrq -> user__information = info2apdu (acb, aci, data, ndata))
		    == NULL)
	goto out2;

    result = encode_ACS_RLRQ__apdu (&acb -> acb_retry, 1, 0, NULLCP, rlrq);

    free_ACS_RLRQ__apdu (rlrq);
    rlrq = NULL;

    if (result == NOTOK) {
	(void) acsaplose (aci, ACS_CONGEST, NULLCP, "error encoding PDU: %s",
			  PY_pepy);
	goto out2;
    }
    acb -> acb_retry -> pe_context = acb -> acb_id;

    PLOGP (acsap_log,ACS_ACSE__apdu, acb -> acb_retry, "RLRQ-apdu", 0);

    result = AcRelRetryRequestAux (acb, secs, acr, aci);
    goto out1;

out2: ;
    if (acb -> acb_retry) {
	pe_free (acb -> acb_retry);
	acb -> acb_retry = NULLPE;
    }
    freeacblk (acb);
    if (rlrq)
	free_ACS_RLRQ__apdu (rlrq);
    result = NOTOK;

out1: ;
    (void) sigiomask (smask);

    return result;
}

/*    A-RELEASE-RETRY.REQUEST (pseudo) */

int	AcRelRetryRequest (sd, secs, acr, aci)
int	sd;
int	secs;
struct AcSAPrelease *acr;
struct AcSAPindication *aci;
{
    SBV	    smask;
    int	    result;
    register struct assocblk *acb;

    missingP (acr);
    missingP (aci);

    smask = sigioblock ();

    if ((acb = findacblk (sd)) == NULL)
	result = acsaplose (aci, ACS_PARAMETER, NULLCP,
			    "invalid association descriptor");
    else
	if (!(acb -> acb_flags & ACB_RELEASE))
	    result = acsaplose (aci, ACS_OPERATION, "release not in progress");
        else
	    result = AcRelRetryRequestAux (acb, secs, acr, aci);

    (void) sigiomask (smask);

    return result;
}

/*  */

static int  AcRelRetryRequestAux (acb, secs, acr, aci)
register struct assocblk *acb;
int	secs;
struct AcSAPrelease *acr;
struct AcSAPindication *aci;
{
    int	    result;
    char   *id = acb -> acb_flags & ACB_RELEASE ? "PRelRetryRequest"
						: "PRelRequest";
    PE	    pe;
    struct PSAPrelease prs;
    register struct PSAPrelease *pr = &prs;
    struct PSAPindication pis;
    register struct PSAPabort  *pa = &pis.pi_abort;
    struct type_ACS_ACSE__apdu *pdu = NULL;
    register struct type_ACS_RLRE__apdu *rlre;

    bzero ((char *) pr, sizeof *pr);

    if ((result = (acb -> acb_flags & ACB_RELEASE)
			? PRelRetryRequest (acb -> acb_fd, secs, pr, &pis)
			: PRelRequest (acb -> acb_fd, &acb -> acb_retry, 1,
				       secs, pr, &pis)) == NOTOK) {
	if (pa -> pa_reason == PC_TIMER) {
	    acb -> acb_flags |= ACB_RELEASE;

	    return ps2acslose (NULLACB, aci, id, pa);
	}

	if (pa -> pa_peer) {
	    (void) AcABORTser (acb -> acb_fd, pa, aci);
	    goto out1;
	}
	if (PC_FATAL (pa -> pa_reason)) {
	    (void) ps2acslose (acb, aci, id, pa);
	    goto out2;
	}
	else {
	    (void) ps2acslose (NULLACB, aci, id, pa);
	    goto out1;
	}
    }

    bzero ((char *) acr, sizeof *acr);
    
    if (pr -> pr_ninfo == 0) {
	result = acsaplose (aci, ACS_PROTOCOL, NULLCP,
			    "no user-data on P-RELEASE");
	goto out3;
    }

    result = decode_ACS_ACSE__apdu (pe = pr -> pr_info[0], 1, NULLIP, NULLVP,
				    &pdu);

#ifdef	DEBUG
    if (result == OK && (acsap_log -> ll_events & LLOG_PDUS))
	pvpdu (acsap_log, print_ACS_ACSE__apdu_P, pe, "ACSE-apdu", 1);
#endif

    pe_free (pe);
    pe = pr -> pr_info[0] = NULLPE;
    
    if (result == NOTOK) {
	(void) acpktlose (acb, aci, ACS_PROTOCOL, NULLCP, "%s", PY_pepy);
	goto out3;
    }

    if (pdu -> offset != type_ACS_ACSE__apdu_rlre) {
	(void) acpktlose (acb, aci, ACS_PROTOCOL, NULLCP,
			  "unexpected PDU %d on P-RELEASE", pdu -> offset);
	goto out3;
    }

    rlre = pdu -> un.rlre;    
    if (rlre -> optionals & opt_ACS_RLRE__apdu_reason)
	acr -> acr_reason = rlre -> reason;
    else
	acr -> acr_reason = int_ACS_reason_normal;
    if (apdu2info (acb, aci, rlre -> user__information, acr -> acr_info,
		   &acr -> acr_ninfo) == NOTOK)
	goto out3;

    if (acr -> acr_affirmative = pr -> pr_affirmative) {
	acb -> acb_fd = NOTOK;
	result = OK;
    }
    else
	result = DONE;

out3: ;
    PRFREE (pr);
    
out2: ;
    if (result == DONE)
	result = OK;
    else
	freeacblk (acb), acb = NULLACB;
out1: ;
    if (acb && acb -> acb_retry) {
	pe_free (acb -> acb_retry);
	acb -> acb_retry = NULLPE;
    }
    if (pdu)
	free_ACS_ACSE__apdu (pdu);

    return result;
}

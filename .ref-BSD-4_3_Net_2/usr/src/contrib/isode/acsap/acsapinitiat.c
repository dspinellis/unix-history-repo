/* acsapinitiat.c - ACPM: initiator */

#ifndef	lint
static char *rcsid = "$Header: /f/osi/acsap/RCS/acsapinitiat.c,v 7.4 91/02/22 09:14:09 mrose Interim $";
#endif

/* 
 * $Header: /f/osi/acsap/RCS/acsapinitiat.c,v 7.4 91/02/22 09:14:09 mrose Interim $
 *
 *
 * $Log:	acsapinitiat.c,v $
 * Revision 7.4  91/02/22  09:14:09  mrose
 * Interim 6.8
 * 
 * Revision 7.3  90/08/18  00:48:49  mrose
 * touch-up
 * 
 * Revision 7.2  90/07/09  14:30:32  mrose
 * sync
 * 
 * Revision 7.1  90/07/01  21:01:55  mrose
 * pepsy
 * 
 * Revision 7.0  89/11/23  21:21:50  mrose
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
#include "isoservent.h"
#include "tailor.h"

/*    A-(ASYN-)ASSOCIATE.REQUEST */

int	AcAsynAssocRequest (context, callingtitle, calledtitle, callingaddr,
	calledaddr, ctxlist, defctxname, prequirements, srequirements, isn,
	settings, ref, data, ndata, qos, acc, aci, async)
OID	context;
AEI	callingtitle,
	calledtitle;
struct PSAPaddr *callingaddr,
		*calledaddr;
int	prequirements,
	srequirements,
	settings,
	ndata,
	async;
long	isn;
struct PSAPctxlist *ctxlist;
OID	defctxname;
struct SSAPref *ref;
PE    *data;
struct QOStype *qos;
struct AcSAPconnect *acc;
struct AcSAPindication *aci;
{
    SBV     smask;
    int     result;

    isodetailor (NULLCP, 0);

    missingP (context);
#ifdef	notdef
    missingP (callingtitle);
    missingP (calledtitle);
#endif

/* let presentation provider catch errors in presentation parameters */
/* except this one... */
    missingP (ctxlist);

    toomuchP (data, ndata, NACDATA, "initial");
    if (data) {	    /* XXX: probably should have a more intensive check... */
	register int    i;
	register PE    *pep;

	for (pep = data, i = ndata; i > 0; pep++, i--)
	    if ((*pep) -> pe_context == PE_DFLT_CTX)
		return acsaplose (aci, ACS_PARAMETER, NULLCP,
			"default context not allowed for user-data at slot %d",
				  pep - data);
    }
    missingP (acc);
    missingP (aci);

    smask = sigioblock ();

    result = AcAssocRequestAux (context, callingtitle, calledtitle,
	    callingaddr, calledaddr, ctxlist, defctxname, prequirements,
	    srequirements, isn, settings, ref, data, ndata, qos, acc, aci,
	    async);

    (void) sigiomask (smask);

    return result;
}

/*  */

static int AcAssocRequestAux (context, callingtitle, calledtitle, callingaddr,
	calledaddr, ctxlist, defctxname, prequirements, srequirements, isn,
	settings, ref, data, ndata, qos, acc, aci, async)
OID	context;
AEI	callingtitle,
	calledtitle;
struct PSAPaddr *callingaddr,
		*calledaddr;
int	prequirements,
	srequirements,
	settings,
	ndata,	
	async;
long	isn;
struct PSAPctxlist *ctxlist;
OID	defctxname;
struct SSAPref *ref;
PE    *data;
struct QOStype *qos;
struct AcSAPconnect *acc;
struct AcSAPindication *aci;
{
    register int    i;
    int	    result;
    PE	    pe;
    register struct assocblk *acb;
    register struct PSAPcontext *pp;
    register struct PSAPconnect *pc = &acc -> acc_connect;
    struct PSAPindication pis;
    register struct PSAPindication *pi = &pis;
    register struct PSAPabort *pa = &pi -> pi_abort;
    register struct type_ACS_AARQ__apdu *pdu;

    if ((acb = newacblk ()) == NULL)
	return acsaplose (aci, ACS_CONGEST, NULLCP, "out of memory");

    pe = NULLPE;
    if ((pdu = (struct type_ACS_AARQ__apdu *) calloc (1, sizeof *pdu))
	    == NULL) {
no_mem: ;
	result = acsaplose (aci, ACS_CONGEST, NULLCP, "out of memory");
	goto out;
    }
    pdu -> application__context__name = context;
    if (calledtitle) {
	pdu -> called__AP__title = calledtitle -> aei_ap_title;
	pdu -> called__AE__qualifier = calledtitle -> aei_ae_qualifier;
	if (calledtitle -> aei_flags & AEI_AP_ID)
	    pdu -> called__AP__invocation__id =
		(struct type_ACS_AP__invocation__id *)
		    &calledtitle -> aei_ap_id;
	if (calledtitle -> aei_flags & AEI_AE_ID)
	    pdu -> called__AE__invocation__id =
	    	(struct type_ACS_AE__invocation__id *)
	    	    &calledtitle -> aei_ae_id;
    }
    if (callingtitle) {
	pdu -> calling__AP__title = callingtitle -> aei_ap_title;
	pdu -> calling__AE__qualifier = callingtitle -> aei_ae_qualifier;
	if (callingtitle -> aei_flags & AEI_AP_ID)
	    pdu -> calling__AP__invocation__id =
		(struct type_ACS_AP__invocation__id *)
		    &callingtitle -> aei_ap_id;
	if (callingtitle -> aei_flags & AEI_AE_ID)
	    pdu -> calling__AE__invocation__id =
	    	(struct type_ACS_AE__invocation__id *)
	    	    &callingtitle -> aei_ae_id;
    }
    if (data
	    && ndata > 0
	    && (pdu -> user__information = info2apdu (acb, aci, data, ndata))
			== NULL)
	goto out;

    result = encode_ACS_AARQ__apdu (&pe, 1, 0, NULLCP, pdu);

    if (pdu -> user__information)
	free_ACS_Association__information (pdu -> user__information);
    free ((char *) pdu);
    pdu = NULL;

    if (result == NOTOK) {
	(void) acsaplose (aci, ACS_CONGEST, NULLCP, "error encoding PDU: %s",
			  PY_pepy);
	goto out;
    }

    if (ctxlist -> pc_nctx >= NPCTX) {
	result = acsaplose (aci, ACS_PARAMETER, NULLCP,
			    "too many contexts");
	goto out;
    }

    {
	register int ctx;
	register OID oid;

	if ((oid = ode2oid (AC_ASN)) == NULLOID) {
	    result = acsaplose (aci, ACS_PARAMETER, NULLCP,
				"%s: unknown", AC_ASN);
	    goto out;
	}

	for (pp = ctxlist -> pc_ctx, i = ctxlist -> pc_nctx - 1;
		i >= 0;
		pp++, i--)
	    if (oid_cmp (pp -> pc_asn, oid)) {
		if (acb -> acb_rosid == PE_DFLT_CTX)
		    acb -> acb_rosid = pp -> pc_id;
		break;
	    }

	ctx = 1;
	for (pp = ctxlist -> pc_ctx, i = ctxlist -> pc_nctx - 1;
	         i >= 0;
	         i--, pp++) {
	    if (oid_cmp (pp -> pc_asn, oid) == 0) {
		acb -> acb_id = pp -> pc_id;
		acb -> acb_offset = pp - ctxlist -> pc_ctx;

		pp = NULL;
		goto ready;
	    }

	    if (ctx <= pp -> pc_id)
		ctx = pp -> pc_id + 2;
	}
	pp -> pc_id = ctx;
	if ((pp -> pc_asn = oid_cpy (oid)) == NULLOID)
	    goto no_mem;
	if (pp -> pc_atn = ode2oid (BER))
	    pp -> pc_atn = oid_cpy (pp -> pc_atn);

	acb -> acb_id = pp -> pc_id;
	acb -> acb_offset = -1;

	ctxlist -> pc_nctx++;
    }
ready: ;
    pe -> pe_context = acb -> acb_id;

    PLOGP (acsap_log,ACS_ACSE__apdu, pe, "AARQ-apdu", 0);

    bzero ((char *) acc, sizeof *acc);

    result = PAsynConnRequest (callingaddr, calledaddr,
	    ctxlist, defctxname, prequirements, srequirements, isn,
	    settings, ref, &pe, 1, qos, pc, pi, async);
    
    if (pp) {
	oid_free (pp -> pc_asn);
	if (pp -> pc_atn)
	    oid_free (pp -> pc_atn);
	pp -> pc_asn = pp -> pc_atn = NULLOID;
	ctxlist -> pc_nctx--;
    }

    pe_free (pe);
    pe = NULLPE;

    if (result == NOTOK) {
	(void) ps2acslose (NULLACB, aci, "PAsynConnRequest", pa);
	goto out;
    }

    acb -> acb_fd = pc -> pc_sd;
    acb -> acb_flags |= ACB_ACS;
    acb -> acb_uabort = PUAbortRequest;

    if (async) {
	switch (result) {
	case CONNECTING_1:
	case CONNECTING_2:
	    acc -> acc_sd = acb -> acb_fd;
	    return result;
	}
    }
    if ((result = AcAsynRetryAux (acb, pc, pi, acc, aci)) == DONE && !async)
	result = OK;
    return result;
    
out: ;
    if (pdu) {
	if (pdu -> user__information)
	    free_ACS_Association__information (pdu -> user__information);
	free ((char *) pdu);
    }
    if (pe)
	pe_free (pe);

    freeacblk (acb);

    return result;
}

/*    A-ASYN-RETRY.REQUEST (pseudo) */

int	AcAsynRetryRequest (sd, acc, aci)
int	sd;
struct AcSAPconnect *acc;
struct AcSAPindication *aci;
{
    SBV     smask;
    int     result;
    register struct assocblk *acb;
    register struct PSAPconnect *pc;
    struct PSAPindication pis;
    register struct PSAPindication *pi = &pis;
    register struct PSAPabort *pa = &pi -> pi_abort;

    missingP (acc);
    missingP (aci);

    smask = sigioblock ();

    if ((acb = findacblk (sd)) == NULL) {
	(void) sigiomask (smask);
	return acsaplose (aci, ACS_PARAMETER, NULLCP,
		"invalid association descriptor");
    }
    if (acb -> acb_flags & ACB_CONN) {
	(void) sigiomask (smask);
	return acsaplose (aci, ACS_OPERATION, NULLCP,
		"association descriptor connected");
    }

    pc = &acc -> acc_connect;
    bzero ((char *) acc, sizeof *acc);

    switch (result = PAsynRetryRequest (acb -> acb_fd, pc, pi)) {
	case NOTOK: 
	    acb -> acb_fd = NOTOK;
	    (void) ps2acslose (acb, aci, "PAsynRetryRequest", pa);
	    freeacblk (acb);
	    break;

	case CONNECTING_1:
	case CONNECTING_2:
	    break;

	case DONE: 
	    result = AcAsynRetryAux (acb, pc, pi, acc, aci);
	    break;
    }

    (void) sigiomask (smask);

    return result;
}

/*  */

static int  AcAsynRetryAux (acb, pc, pi, acc, aci)
register struct assocblk *acb;
struct PSAPconnect *pc;
struct PSAPindication *pi;
struct AcSAPconnect *acc;
struct AcSAPindication *aci;
{
    register int    i;
    int	    result;
    PE	    pe;
    register struct PSAPcontext *pp;
    register struct PSAPabort *pa = &pi -> pi_abort;
    struct type_ACS_ACSE__apdu *pdu;
    register struct type_ACS_AARE__apdu *aare;

    if (pc -> pc_result == PC_ABORTED) {
	(void) ps2acsabort (acb, pa, aci);

	acc -> acc_sd = NOTOK;
	acc -> acc_result = ACS_ABORTED;

	return DONE;
    }

    pe = NULLPE;
    pdu = NULL;

    if (pc -> pc_ninfo < 1) {
	if (pc -> pc_result != PC_ACCEPT) {
	    bzero ((char *) pa, sizeof *pa);
	    pa -> pa_reason = pc -> pc_result;
	    acb -> acb_fd = NOTOK;
	    (void) ps2acslose (acb, aci, "PAsynConnRequest(pseudo)", pa);

	    acc -> acc_sd = NOTOK;
	    acc -> acc_result = aci -> aci_abort.aca_reason;

	    result = DONE;
	}
	else
	    result = acpktlose (acb, aci, ACS_PROTOCOL, NULLCP, NULLCP);
	goto out;
    }

    acb -> acb_fd = pc -> pc_sd;
    acb -> acb_sversion = pc -> pc_qos.qos_sversion;
    
    result = decode_ACS_ACSE__apdu (pe = pc -> pc_info[0], 1, NULLIP, NULLVP,
				    &pdu);

#ifdef	DEBUG
    if (result == OK && (acsap_log -> ll_events & LLOG_PDUS))
	pvpdu (acsap_log, print_ACS_ACSE__apdu_P, pe, "ACSE-apdu", 1);
#endif

    pe_free (pe);
    pe = pc -> pc_info[0] = NULLPE;

    if (result == NOTOK) {
	(void) acpktlose (acb, aci, ACS_PROTOCOL, NULLCP, "%s", PY_pepy);
	goto out;
    }

    if (pdu -> offset != type_ACS_ACSE__apdu_aare) {
	result = acpktlose (acb, aci, ACS_PROTOCOL, NULLCP,
			    "unexpected PDU %d on P-CONNECT", pdu -> offset);
	goto out;
    }

    aare = pdu -> un.aare;
    switch (aare -> result) {
	case int_ACS_result_accepted:
	    if (pc -> pc_result != PC_ACCEPT) {
		result = acpktlose (acb, aci, ACS_PROTOCOL, NULLCP,
				    "not accepted [%s]",
				    PErrString (pc -> pc_result));
		goto out;
	    }

	    acb -> acb_flags |= ACB_CONN;

	    acc -> acc_sd = acb -> acb_fd;
	    acc -> acc_result = ACS_ACCEPT;

	    if ((i = acb -> acb_offset) < 0)
		i = pc -> pc_ctxlist.pc_nctx - 1;
	    pp = pc -> pc_ctxlist.pc_ctx + i;
	    if (pp -> pc_id != acb -> acb_id) {
		result = acpktlose (acb, aci, ACS_PROTOCOL, NULLCP,
				    "ACSE PCI not found");
		goto out;
	    }
	    if (pp -> pc_result != PC_ACCEPT) {
		result = acpktlose (acb, aci, ACS_PROTOCOL, NULLCP,
				    "ACSE PCI rejected");
		goto out;
	    }

	    if (acb -> acb_offset < 0)
		pc -> pc_ctxlist.pc_nctx--;

	    for (pp = pc -> pc_ctxlist.pc_ctx; i >= 0; i--, pp++)
		if (pp -> pc_id != acb -> acb_id
			&& pp -> pc_result == PC_ACCEPT) {
		    acb -> acb_rosid = pp -> pc_id;
		    break;
		}
	    break;
				  
	case int_ACS_result_rejected__permanent:
	    acc -> acc_result = ACS_PERMANENT;
	    goto rejected;

	case int_ACS_result_rejected__transient:
	    acc -> acc_result = ACS_TRANSIENT;
rejected: ;
	    if (pc -> pc_result != PC_ACCEPT)
		acb -> acb_fd = NOTOK;
	    acc -> acc_sd = NOTOK;
	    if (acb -> acb_offset < 0
		    && (i = pc -> pc_ctxlist.pc_nctx - 1) >= 0)
		pc -> pc_ctxlist.pc_nctx = i;
	    break;
    }

    switch (aare -> result__source__diagnostic -> offset) {
	case type_ACS_Associate__source__diagnostic_acse__service__user:
	    acc -> acc_diagnostic =
		aare -> result__source__diagnostic -> un.acse__service__user
		    + ACS_USER_BASE;
	    break;

	case type_ACS_Associate__source__diagnostic_acse__service__provider:
	default:
	    acc -> acc_diagnostic =
		aare -> result__source__diagnostic -> un.acse__service__provider
		    + ACS_PROV_BASE;
	    break;
    }

    if ((result = apdu2info (acb, aci, aare -> user__information,
			     acc -> acc_info, &acc -> acc_ninfo)) == NOTOK)
	goto out;

    acc -> acc_context = aare -> application__context__name;
    aare -> application__context__name = NULLOID;
    acc -> acc_respondtitle.aei_ap_title = aare -> responding__AP__title;
    aare -> responding__AP__title = NULLPE;
    acc -> acc_respondtitle.aei_ae_qualifier =
	aare -> responding__AE__qualifier;
    aare -> responding__AE__qualifier = NULLPE;
    if (aare -> responding__AP__invocation__id) {
	acc -> acc_respondtitle.aei_ap_id =
	    aare -> responding__AP__invocation__id -> parm;
	acc -> acc_respondtitle.aei_flags |= AEI_AP_ID;
    }
    if (aare -> responding__AE__invocation__id) {
	acc -> acc_respondtitle.aei_ae_id =
	    aare -> responding__AE__invocation__id -> parm;
	acc -> acc_respondtitle.aei_flags |= AEI_AE_ID;
    }

    for (i = pc -> pc_ninfo - 1; i >= 0; i--)
	if (pc -> pc_info[i]) {
	    pe_free (pc -> pc_info[i]);
	    pc -> pc_info[i] = NULL;
	}
    pc -> pc_ninfo = 0;

    free_ACS_ACSE__apdu (pdu);

    if (pc -> pc_result != PC_ACCEPT)
	freeacblk (acb);

    return DONE;
    
out: ;
    if (pc -> pc_ninfo > 0 && pe == pc -> pc_info[0])
	pe = NULLPE;
    PCFREE (pc);
    if (pe)
	pe_free (pe);
    if (pdu)
	free_ACS_ACSE__apdu (pdu);

    freeacblk (acb);

    return result;
}

/*    A-ASYN-NEXT.REQUEST (pseudo) */

int	AcAsynNextRequest (sd, acc, aci)
int	sd;
struct AcSAPconnect *acc;
struct AcSAPindication *aci;
{
    SBV     smask;
    int     result;
    register struct assocblk *acb;
    register struct PSAPconnect *pc;
    struct PSAPindication pis;
    register struct PSAPindication *pi = &pis;
    register struct PSAPabort *pa = &pi -> pi_abort;

    missingP (acc);
    missingP (aci);

    smask = sigioblock ();

    if ((acb = findacblk (sd)) == NULL) {
	(void) sigiomask (smask);
	return acsaplose (aci, ACS_PARAMETER, NULLCP,
		"invalid association descriptor");
    }
    if (acb -> acb_flags & ACB_CONN) {
	(void) sigiomask (smask);
	return acsaplose (aci, ACS_OPERATION, NULLCP,
		"association descriptor connected");
    }

    pc = &acc -> acc_connect;
    bzero ((char *) acc, sizeof *acc);

    switch (result = PAsynNextRequest (acb -> acb_fd, pc, pi)) {
	case NOTOK: 
	    acb -> acb_fd = NOTOK;
	    (void) ps2acslose (acb, aci, "PAsynRetryRequest", pa);
	    freeacblk (acb);
	    break;

	case CONNECTING_1:
	case CONNECTING_2:
	    break;

	case DONE: 
	    result = AcAsynRetryAux (acb, pc, pi, acc, aci);
	    break;
    }

    (void) sigiomask (smask);

    return result;
}

/* acsaprespond.c - ACPM: responder */

#ifndef	lint
static char *rcsid = "$Header: /f/osi/acsap/RCS/acsaprespond.c,v 7.4 91/02/22 09:14:16 mrose Interim $";
#endif

/* 
 * $Header: /f/osi/acsap/RCS/acsaprespond.c,v 7.4 91/02/22 09:14:16 mrose Interim $
 *
 *
 * $Log:	acsaprespond.c,v $
 * Revision 7.4  91/02/22  09:14:16  mrose
 * Interim 6.8
 * 
 * Revision 7.3  90/11/04  19:14:31  mrose
 * update
 * 
 * Revision 7.2  90/07/09  14:30:38  mrose
 * sync
 * 
 * Revision 7.1  90/07/01  21:02:08  mrose
 * pepsy
 * 
 * Revision 7.0  89/11/23  21:21:57  mrose
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
#include "ACS-types.h"
#define	ACSE
#include "acpkt.h"
#include "tailor.h"

/*    A-ASSOCIATE.INDICATION */

int	AcInit (vecp, vec, acs, aci)
int	vecp;
char  **vec;
struct AcSAPstart *acs;
struct AcSAPindication *aci;
{
    register int    i;
    int	    ctx,
	    result;
    register struct assocblk *acb;
    register struct PSAPstart *ps;
    struct PSAPindication   pis;
    register struct PSAPindication *pi = &pis;
    register struct PSAPabort  *pa = &pi -> pi_abort;
    PE	    pe = NULLPE;
    struct type_ACS_ACSE__apdu *pdu;
    register struct type_ACS_AARQ__apdu *aarq;

    isodetailor (NULLCP, 0);

    missingP (vec);
    missingP (acs);
    missingP (aci);

    ps = &acs -> acs_start;
    if ((acb = newacblk ()) == NULL)
	return acsaplose (aci, ACS_CONGEST, NULLCP, "out of memory");

    bzero ((char *) acs, sizeof *acs);

    if (PInit (vecp, vec, ps, pi) == NOTOK) {
	(void) ps2acslose (acb, aci, "PInit", pa);
	goto out1;
    }

    acb -> acb_flags |= ACB_ACS;
    acb -> acb_fd = ps -> ps_sd;
    acb -> acb_sversion = ps -> ps_qos.qos_sversion;
    acb -> acb_uabort = PUAbortRequest;

    pdu = NULL;
    if (ps -> ps_ninfo < 1) {
	(void) acsaplose (aci, ACS_PROTOCOL, NULLCP,
			  "no user-data on P-CONNECT");
	goto out2;
    }

    result = decode_ACS_ACSE__apdu (pe = ps -> ps_info[0], 1, NULLIP, NULLVP,
				    &pdu);

#ifdef	DEBUG
    if (result == OK && (acsap_log -> ll_events & LLOG_PDUS))
	pvpdu (acsap_log, print_ACS_ACSE__apdu_P, pe, "ACSE-apdu", 1);
#endif

    ctx = pe -> pe_context;

    pe_free (pe);
    pe = ps -> ps_info[0] = NULLPE;
    
    if (result == NOTOK) {
	(void) acsaplose (aci, ACS_PROTOCOL, NULLCP, "%s", PY_pepy);
	goto out2;
    }

    if (pdu -> offset != type_ACS_ACSE__apdu_aarq) {
	(void) acsaplose (aci, ACS_PROTOCOL, NULLCP,
			  "unexpected PDU %d on P-CONNECT", pdu -> offset);
	goto out2;
    }

    aarq = pdu -> un.aarq;

    if ((acb -> acb_context = oid_cpy (aarq -> application__context__name))
	    == NULLOID) {
	(void) acsaplose (aci, ACS_CONGEST, NULLCP, NULLCP);
	goto out2;
    }

    {
	register OID	oid;
	register struct PSAPcontext *pp;

	if ((oid = ode2oid (AC_ASN)) == NULLOID) {
	    (void) acsaplose (aci, ACS_PARAMETER, NULLCP,
			      "%s: unknown", AC_ASN);
	    goto out2;
	}

	for (pp = ps -> ps_ctxlist.pc_ctx, i = ps -> ps_ctxlist.pc_nctx - 1;
	         i >= 0;
	         pp++, i--)
	    if (pp -> pc_id == ctx) {
		if (oid_cmp (pp -> pc_asn, oid)) {
		    (void) acsaplose (aci, ACS_PROTOCOL, NULLCP,
				      "wrong ASN for ACSE");
		    goto out2;
		}
		if (pp -> pc_result != PC_ACCEPT) {
		    (void) acsaplose (aci, ACS_PROTOCOL, NULLCP,
				      "PCI for ACSE not accepted");
		    goto out2;
		}

		acb -> acb_id = ctx;
	    }
	    else
		if (acb -> acb_rosid == PE_DFLT_CTX)
		    acb -> acb_rosid = pp -> pc_id;

	if (acb -> acb_id == PE_DFLT_CTX) {
	    (void) acsaplose (aci, ACS_PROTOCOL, NULLCP,
			      "unable to find PCI for ACSE");
	    goto out2;
	}
    }

    acs -> acs_sd = acb -> acb_fd;

    if (apdu2info (acb, aci, aarq -> user__information, acs -> acs_info,
		   &acs -> acs_ninfo) == NOTOK)
	goto out2;

    acs -> acs_context = aarq -> application__context__name;
    aarq -> application__context__name = NULLOID;
    acs -> acs_callingtitle.aei_ap_title = aarq -> calling__AP__title;
    aarq -> calling__AP__title = NULLPE;
    acs -> acs_callingtitle.aei_ae_qualifier =
	aarq -> calling__AE__qualifier;
    aarq -> calling__AE__qualifier = NULLPE;
    if (aarq -> calling__AP__invocation__id) {
	acs -> acs_callingtitle.aei_ap_id =
	    aarq -> calling__AP__invocation__id -> parm;
	acs -> acs_callingtitle.aei_flags |= AEI_AP_ID;
    }
    if (aarq -> calling__AE__invocation__id) {
	acs -> acs_callingtitle.aei_ae_id =
	    aarq -> calling__AE__invocation__id -> parm;
	acs -> acs_callingtitle.aei_flags |= AEI_AE_ID;
    }
    acs -> acs_calledtitle.aei_ap_title = aarq -> called__AP__title;
    aarq -> called__AP__title = NULLPE;
    acs -> acs_calledtitle.aei_ae_qualifier =
	aarq -> called__AE__qualifier;
    aarq -> called__AE__qualifier = NULLPE;
    if (aarq -> called__AP__invocation__id) {
	acs -> acs_calledtitle.aei_ap_id =
	    aarq -> called__AP__invocation__id -> parm;
	acs -> acs_calledtitle.aei_flags |= AEI_AP_ID;
    }
    if (aarq -> called__AE__invocation__id) {
	acs -> acs_calledtitle.aei_ae_id =
	    aarq -> called__AE__invocation__id -> parm;
	acs -> acs_calledtitle.aei_flags |= AEI_AE_ID;
    }

    for (i = ps -> ps_ninfo - 1; i >= 0; i--)
	if (ps -> ps_info[i]) {
	    pe_free (ps -> ps_info[i]);
	    ps -> ps_info[i] = NULL;
	}
    ps -> ps_ninfo = 0;

    free_ACS_ACSE__apdu (pdu);

    return OK;
    
out2: ;
    if (pdu)
	free_ACS_ACSE__apdu (pdu);

/* XXX: should do AARE APDU, but can't given any useful info... */
    (void) PConnResponse (ps -> ps_sd, PC_REJECTED, NULLPA, &ps -> ps_ctxlist,
			  ps -> ps_defctxresult, 0, 0, SERIAL_NONE, 0,
			  &ps -> ps_connect, NULLPEP, 0, &pis);

    PSFREE (ps);

out1: ;
    freeacblk (acb);

    return NOTOK;
}

/*    A-ASSOCIATE.RESPONSE */

int	AcAssocResponse (sd, status, reason, context, respondtitle,
	respondaddr, ctxlist, defctxresult, prequirements, srequirements, isn,
	settings, ref, data, ndata, aci)
int	sd;
int	status,
    	reason;
OID	context;
AEI	respondtitle;
struct PSAPaddr *respondaddr;
int	prequirements,
	srequirements,
	settings,
	ndata;
long	isn;
struct PSAPctxlist *ctxlist;
int	defctxresult;
struct SSAPref *ref;
PE     *data;
struct AcSAPindication *aci;
{
    int	    pstatus,
	    result;
    PE	    pe;
    register struct assocblk   *acb;
    struct PSAPindication pis;
    register struct PSAPindication *pi = &pis;
    register struct PSAPabort *pa = &pi -> pi_abort;
    register struct type_ACS_AARE__apdu *pdu;

    if ((acb = findacblk (sd)) == NULL || (acb -> acb_flags & ACB_CONN))
	return acsaplose (aci, ACS_PARAMETER, NULLCP,
		"invalid association descriptor");
    switch (status) {
	case ACS_ACCEPT: 
	    pstatus = PC_ACCEPT;
	    if (reason != ACS_USER_NULL) {
bad_reason: ;
		return acsaplose (aci, ACS_PARAMETER, NULLCP,
				  "invalid value for reason parameter");
	    }
	    break;

	case ACS_PERMANENT: 
	case ACS_TRANSIENT: 
	    pstatus = PC_REJECTED;
	    switch (reason) {
		case ACS_USER_NOREASON:
		case ACS_CONTEXT:
		case ACS_CALLING_AP_TITLE:
		case ACS_CALLING_AP_ID:
		case ACS_CALLING_AE_QUAL:
		case ACS_CALLING_AE_ID:
		case ACS_CALLED_AP_TITLE:
		case ACS_CALLED_AP_ID:
		case ACS_CALLED_AE_QUAL:
		case ACS_CALLED_AE_ID:
		    break;

		default:
		    goto bad_reason;
	    }
	    break;

	default: 
	    return acsaplose (aci, ACS_PARAMETER, NULLCP,
		    "bad value for status parameter");
    }
#ifdef	notdef
    missingP (context);
    missingP (respondtitle);
#endif

/* let presentation provider catch errors in presentation parameters */

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
    missingP (aci);

    pe = NULLPE;
    if ((pdu = (struct type_ACS_AARE__apdu *) calloc (1, sizeof *pdu))
	    == NULL) {
no_mem: ;
	(void) acsaplose (aci, ACS_CONGEST, NULLCP, "out of memory");
	goto out2;
    }
    pdu -> application__context__name = context ? context : acb -> acb_context;
    pdu -> result = status;
    if ((pdu -> result__source__diagnostic = 
		(struct type_ACS_Associate__source__diagnostic *) malloc
	  (sizeof (struct type_ACS_Associate__source__diagnostic))) == NULL)
	goto no_mem;
    pdu -> result__source__diagnostic -> offset =
	type_ACS_Associate__source__diagnostic_acse__service__user;
    pdu -> result__source__diagnostic -> un.acse__service__user =
	reason - ACS_USER_BASE;
    if (respondtitle) {
	pdu -> responding__AP__title = respondtitle -> aei_ap_title;
	pdu -> responding__AE__qualifier = respondtitle -> aei_ae_qualifier;
	if (respondtitle -> aei_flags & AEI_AP_ID)
	    pdu -> responding__AP__invocation__id =
		(struct type_ACS_AP__invocation__id *)
		    &respondtitle -> aei_ap_id;
	if (respondtitle -> aei_flags & AEI_AE_ID)
	    pdu -> responding__AE__invocation__id =
	    	(struct type_ACS_AE__invocation__id *)
	    	    &respondtitle -> aei_ae_id;
    }
    if (data
	    && ndata > 0
	    && (pdu -> user__information = info2apdu (acb, aci, data, ndata))
			== NULL)
	goto out2;

    result = encode_ACS_AARE__apdu (&pe, 1, 0, NULLCP, pdu);

    free_ACS_Associate__source__diagnostic (pdu -> result__source__diagnostic);
    if (pdu -> user__information)
	free_ACS_Association__information (pdu -> user__information);
    free ((char *) pdu);
    pdu = NULL;

    if (result == NOTOK) {
	(void) acsaplose (aci, ACS_CONGEST, NULLCP, "error encoding PDU: %s",
			  PY_pepy);
	goto out2;
    }

    if (ctxlist) {
	register int	i;
	register struct PSAPcontext *pp;

	if (ctxlist -> pc_nctx > NPCTX) {
	    (void) acsaplose (aci, ACS_PARAMETER, NULLCP, "too many contexts");
	    goto out1;
	}

	for (pp = ctxlist -> pc_ctx, i = ctxlist -> pc_nctx - 1;
		    i >= 0;
		    i--, pp++)
	    if (acb -> acb_id == pp -> pc_id) {
		if (pp -> pc_result != PC_ACCEPT) {
		    (void) acsaplose (aci, ACS_PARAMETER, NULLCP,
				      "PCI for ACSE not accepted");
		    goto out1;
		}
	    }
	    else
		if (pp -> pc_result == PC_ACCEPT)
		    acb -> acb_rosid = pp -> pc_id;
    }
    pe -> pe_context = acb -> acb_id;

    PLOGP (acsap_log,ACS_ACSE__apdu, pe, "AARE-apdu", 0);

    result = PConnResponse (acb -> acb_fd, pstatus, respondaddr,
		ctxlist, defctxresult, prequirements, srequirements, isn,
		settings, ref, &pe, 1, pi);

    pe_free (pe);
    pe = NULLPE;

    if (result == NOTOK) {
	(void) ps2acslose (acb, aci, "PConnResponse", pa);
	if (PC_FATAL (pa -> pa_reason))
	    goto out2;
	else
	    goto out1;
    }
    
    if (status == ACS_ACCEPT)
	acb -> acb_flags |= ACB_CONN;
    else {
	acb -> acb_fd = NOTOK;
	freeacblk (acb);
    }

    return OK;
    
out2: ;
    freeacblk (acb);
out1: ;
    if (pdu) {
	if (pdu -> result__source__diagnostic)
	    free_ACS_Associate__source__diagnostic (pdu -> result__source__diagnostic);
	if (pdu -> user__information)
	    free_ACS_Association__information (pdu -> user__information);
	free ((char *) pdu);
    }
    if (pe)
	pe_free (pe);

    return NOTOK;
}
    

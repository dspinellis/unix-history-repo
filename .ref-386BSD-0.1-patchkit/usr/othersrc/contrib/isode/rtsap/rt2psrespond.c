/* rt2psrespond.c - RTPM: responder */

#ifndef	lint
static char *rcsid = "$Header: /f/osi/rtsap/RCS/rt2psrespond.c,v 7.4 91/02/22 09:42:22 mrose Interim $";
#endif

/* 
 * $Header: /f/osi/rtsap/RCS/rt2psrespond.c,v 7.4 91/02/22 09:42:22 mrose Interim $
 *
 *
 * $Log:	rt2psrespond.c,v $
 * Revision 7.4  91/02/22  09:42:22  mrose
 * Interim 6.8
 * 
 * Revision 7.3  90/10/23  20:44:07  mrose
 * update
 * 
 * Revision 7.2  90/07/27  08:47:50  mrose
 * update
 * 
 * Revision 7.1  90/07/01  21:07:00  mrose
 * pepsy
 * 
 * Revision 6.0  89/03/18  23:43:13  mrose
 * Release 5.0
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
#include "RTS-types.h"
#include "rtpkt.h"
#include "tailor.h"

/*    RT-OPEN.INDICATION */

int	RtInit_Aux (vecp, vec, rts, rti, dctx)
int	vecp;
char  **vec;
struct RtSAPstart *rts;
struct RtSAPindication *rti;
OID	dctx;
{
    int	    ctx,
	    i;
    register PE	    pe;
    register struct assocblk   *acb;
    register struct PSAPstart *ps;
    register struct AcSAPstart *acs;
    struct AcSAPindication  acis;
    register struct AcSAPindication  *aci = &acis;
    register struct AcSAPabort *aca = &aci -> aci_abort;
    struct type_RTS_RTSE__apdus *rtpdu;
    struct type_RTS_RTORQapdu *prtorq;

    isodetailor (NULLCP, 0);

    missingP (vec);
    missingP (rts);
    missingP (rti);

    acs = &rts -> rts_start;
    ps = &acs -> acs_start;

    bzero ((char *) rts, sizeof *rts);

    if (AcInit (vecp, vec, acs, aci) == NOTOK)
	return acs2rtslose (NULLACB, rti, "AcInit", aca);
    if ((acb = findacblk (acs -> acs_sd)) == NULLACB) {
	(void) rtsaplose (rti, RTS_PROTOCOL, NULLCP, "ACSE mangled");
	goto out;
    }
    acb -> acb_flags |= ACB_RTS;
    acb -> acb_uabort = AcUAbortRequest;
    SetPS2RtService (acb);

    acb -> acb_connect = ps -> ps_connect;	/* struct copy */
    if ((ps -> ps_srequirements &= RTS_MYREQUIRE) != RTS_MYREQUIRE) {
	(void) rtsaplose (rti, RTS_PROTOCOL, NULLCP,
		    "desired session requirements unavailable");
	goto out;
    }
    acb -> acb_requirements = ps -> ps_srequirements;

#define dotoken(requires,shift,bit,type) \
{ \
    if (acb -> acb_requirements & requires) \
	switch (ps -> ps_settings & (ST_MASK << shift)) { \
	    case ST_INIT_VALUE << shift: \
		acb -> acb_avail |= bit; \
		break; \
 \
	    case ST_RESP_VALUE << shift: \
		acb -> acb_owned |= bit; \
		acb -> acb_avail |= bit; \
		break; \
 \
	    default: \
		(void) rtsaplose (rti, RTS_PROTOCOL, NULLCP, \
			    "%s token management botched", type); \
		goto out; \
	} \
    }
    dotokens ();
#undef	dotoken
    if (acb -> acb_owned != 0 && acb -> acb_owned != acb -> acb_avail) {
	(void) rtsaplose (rti, RTS_PROTOCOL, NULLCP,
		"token management botched");
	goto out;
    }
    if (acb -> acb_owned)
	acb -> acb_flags |= ACB_TURN;
    acb -> acb_settings = ps -> ps_settings;
    acb -> acb_ssdusize = ps -> ps_ssdusize;

    if ((pe = acs -> acs_info[0]) == NULLPE) {
	(void) rtsaplose (rti, RTS_PROTOCOL, NULLCP, NULLCP);
	goto out;
    }
    if (decode_RTS_RTSE__apdus (pe, 1, NULLIP, NULLVP, &rtpdu) == NOTOK) {
	(void) pylose ();
	goto out;
    }

    PLOGP (rtsap_log,RTS_RTORQapdu, pe, "RTORQapdu", 1);

    if (rtpdu -> offset != type_RTS_RTSE__apdus_rtorq__apdu) {
	(void) rtsaplose (rti, RTS_PROTOCOL, NULLCP, "Unexpected PDU");
	free_RTS_RTSE__apdus (rtpdu);
	goto out;
    }

    prtorq = rtpdu -> un.rtorq__apdu;
    acb -> acb_ckpoint = acb -> acb_ssdusize >> 10;
    if ((0 < prtorq -> checkpointSize
	        && prtorq -> checkpointSize < acb -> acb_ckpoint)
	    || acb -> acb_ckpoint <= 0)
	acb -> acb_ckpoint = prtorq -> checkpointSize;
    acb -> acb_window = prtorq -> windowSize;

    ctx = pe -> pe_context;

    {
	register OID	oid;
	register struct PSAPcontext *pp;

	if (dctx)
	    oid = dctx;
	else if ((oid = ode2oid (RT_ASN)) == NULLOID) {
	    (void) rtsaplose (rti, RTS_PARAMETER, NULLCP,
			      "%s: unknown", RT_ASN);
	    free_RTS_RTSE__apdus(rtpdu);
	    goto out;
	}

	i = ps -> ps_ctxlist.pc_nctx - 1;
	for (pp = ps -> ps_ctxlist.pc_ctx; i >= 0; i--, pp++)
	    if (pp -> pc_id == ctx) {
		if (oid_cmp (pp -> pc_asn, oid)) {
		    (void) rtsaplose (rti, RTS_PROTOCOL, NULLCP,
				      "wrong ASN for RTSE (%s)",
				      sprintoid (pp -> pc_asn));
		    free_RTS_RTSE__apdus(rtpdu);
		    goto out;
		}
		if (pp -> pc_result != PC_ACCEPT) {
		    (void) rtsaplose (rti, RTS_PROTOCOL, NULLCP,
				      "PCI for RTSE not accepted");
		    free_RTS_RTSE__apdus(rtpdu);
		    goto out;
		}

		acb -> acb_rtsid = ctx;
	    }

	if (acb -> acb_rtsid == PE_DFLT_CTX) {
	    (void) rtsaplose (rti, RTS_PROTOCOL, NULLCP,
			      "unable to find PCI for RTSE");
	    free_RTS_RTSE__apdus(rtpdu);
	    goto out;
	}
    }

    rts -> rts_sd = acb -> acb_fd;
    if (prtorq -> dialogueMode == RTORQ_DM_TWA)
	rts -> rts_mode = RTS_TWA, acb -> acb_flags |= ACB_TWA;
    else
	rts -> rts_mode = RTS_MONOLOGUE;
    rts -> rts_turn = acb -> acb_flags & ACB_TURN ? RTS_RESPONDER
	    : RTS_INITIATOR;
    if (prtorq -> connectionDataRQ -> offset == type_RTS_ConnectionData_open) {
	(void) pe_extract(pe, rts -> rts_data = prtorq -> connectionDataRQ
						       -> un.open);
	if (rts -> rts_data)
	    rts -> rts_data -> pe_refcnt ++;
    }
    else
	pe = NULLPE;

    for (i = acs -> acs_ninfo - 1; i >= 0; i--) 
	if (acs -> acs_info[i]) {
	    pe_free (acs -> acs_info[i]);
	    acs -> acs_info[i] = NULLPE;
	}
    acs -> acs_ninfo = 0;

    free_RTS_RTSE__apdus(rtpdu);
    if (dctx) oid_free (dctx);
    return OK;

out: ;
/* XXX: should do RTORQ APDU, but can't give any useful info... */
    (void) AcAssocResponse (acs -> acs_sd, ACS_TRANSIENT, ACS_USER_NOREASON,
		NULLOID, NULLAEI, NULLPA, &ps -> ps_ctxlist,
		ps -> ps_defctxresult, 0, 0, SERIAL_NONE, 0, &ps -> ps_connect,
		NULLPEP, 0, aci);

    ACSFREE (acs);
    if (dctx) oid_free (dctx);
    return NOTOK;    
}

/*    RT-OPEN.RESPONSE */

int	RtOpenResponse (sd, status, context, respondtitle, respondaddr,
	ctxlist, defctxresult, data, rti)
int	sd,
	status;
AEI	respondtitle;
OID	context;
struct PSAPaddr *respondaddr;
struct PSAPctxlist *ctxlist;
int	defctxresult;
PE	data;
struct RtSAPindication *rti;
{
    int	    result;
    PE	    pe,
	    p,
	    q;
    register struct assocblk   *acb;
    struct AcSAPindication acis;
    register struct AcSAPindication *aci = &acis;
    register struct AcSAPabort *aca = &aci -> aci_abort;

    if ((acb = findacblk (sd)) == NULL || (acb -> acb_flags & ACB_CONN))
	return rtsaplose (rti, RTS_PARAMETER, NULLCP,
		"invalid association descriptor");
    if (!(acb -> acb_flags & ACB_ACS) || !(acb -> acb_flags & ACB_RTS))
	return rtsaplose (rti, RTS_OPERATION, NULLCP,
		"not an association descritor for RTS");
    missingP (rti);

    if ((pe = pe_alloc (PE_CLASS_UNIV, PE_FORM_CONS, PE_CONS_SET))
	    == NULLPE) {
no_mem: ;
	(void) rtsaplose (rti, RTS_CONGEST, NULLCP, "out of memory");
	goto out1;
    }
    switch (status) {
	case ACS_ACCEPT:
/* begin RTOAC APDU */
	    pe -> pe_class = PE_CLASS_CONT; pe -> pe_id = 17;
	    if (set_add (pe, num2prim ((integer) acb -> acb_ckpoint,
				       PE_CLASS_CONT, RTOAC_CKPOINT)) == NOTOK
		    || set_add (pe, num2prim ((integer) acb -> acb_window,
					      PE_CLASS_CONT,
					      RTOAC_WINDOW)) == NOTOK
		    || set_add (pe, p = pe_alloc (PE_CLASS_CONT, PE_FORM_CONS,
			    RTOAC_CONNDATA)) == NOTOK
		    || set_add (p, q = pe_alloc (PE_CLASS_CONT, PE_FORM_CONS,
			    RTOAC_CD_OPEN)) == NOTOK
		    || set_add (q, data ? data : pe_alloc (PE_CLASS_UNIV,
			    PE_FORM_PRIM, PE_PRIM_NULL)) == NOTOK)
		goto no_mem;
/* end RTOAC APDU */
	    break;
	    
	default:
/* begin RTORJ APDU */
	    pe -> pe_class = PE_CLASS_CONT; pe -> pe_id = 18;
	    if (set_add (pe, p = pe_alloc (PE_CLASS_CONT, PE_FORM_CONS,
			RTORJ_USERDATA)) == NOTOK
		    || set_add (p, data ? data : pe_alloc (PE_CLASS_UNIV,
			    PE_FORM_PRIM, PE_PRIM_NULL)) == NOTOK)
		goto no_mem;
/* end RTORJ APDU */
	    break;
    }

    if (ctxlist) {
	register int	i;
	register struct PSAPcontext *pp;

	if (ctxlist -> pc_nctx > NPCTX) {
	    (void) rtsaplose (rti, RTS_PARAMETER, NULLCP, "too many contexts");
	    goto out1;
	}

	for (pp = ctxlist -> pc_ctx, i = ctxlist -> pc_nctx - 1;
		    i >= 0;
		    i--, pp++)
	    if (acb -> acb_rtsid == pp -> pc_id) {
		if (pp -> pc_result != PC_ACCEPT) {
		    (void) acsaplose (aci, ACS_PARAMETER, NULLCP,
				      "PCI for RTSE not accepted");
		    goto out1;
		}
	    }
    }
    pe -> pe_context = acb -> acb_rtsid;

#ifdef	DEBUG
    if (rtsap_log -> ll_events & LLOG_PDUS)
	if (status == ACS_ACCEPT)
	    pvpdu (rtsap_log, print_RTS_RTSE__apdus_P, pe, "RTOACapdu", 0);
	else
	    pvpdu (rtsap_log, print_RTS_RTSE__apdus_P, pe, "RTORJapdu", 0);	
#endif

    result = AcAssocResponse (acb -> acb_fd, status,
		status != ACS_ACCEPT ? ACS_USER_NOREASON : ACS_USER_NULL,
		context, respondtitle, respondaddr, ctxlist, defctxresult, 0,
		acb -> acb_requirements, SERIAL_NONE, acb -> acb_settings,
		&acb -> acb_connect, &pe, 1, aci);

    if (result == NOTOK) {
	(void) acs2rtslose (acb, rti, "AcAssocResponse", aca);
	if (ACS_FATAL (aca -> aca_reason))
	    goto out2;
	else
	    goto out1;
    }

    result = OK;

out2:
    if (pe) {
	if (data)
	    (void) pe_extract (pe, data);
	pe_free (pe);
    }

    return result;

out1: ;
    (void) AcAssocResponse (acb -> acb_fd, ACS_TRANSIENT, ACS_USER_NOREASON,
		NULLOID, NULLAEI, NULLPA, NULLPC, PC_ACCEPT, 0, 0, SERIAL_NONE,
		0, &acb -> acb_connect, NULLPEP, 0, aci);

    result = NOTOK;
    goto out2;
}

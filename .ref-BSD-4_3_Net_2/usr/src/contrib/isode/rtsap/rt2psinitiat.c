/* rt2psinitiat.c - RTPM: initiator */

#ifndef	lint
static char *rcsid = "$Header: /f/osi/rtsap/RCS/rt2psinitiat.c,v 7.4 91/02/22 09:42:19 mrose Interim $";
#endif

/* 
 * $Header: /f/osi/rtsap/RCS/rt2psinitiat.c,v 7.4 91/02/22 09:42:19 mrose Interim $
 *
 *
 * $Log:	rt2psinitiat.c,v $
 * Revision 7.4  91/02/22  09:42:19  mrose
 * Interim 6.8
 * 
 * Revision 7.3  90/10/23  20:44:04  mrose
 * update
 * 
 * Revision 7.2  90/07/27  08:47:46  mrose
 * update
 * 
 * Revision 7.1  90/07/01  21:06:52  mrose
 * pepsy
 * 
 * Revision 6.0  89/03/18  23:43:09  mrose
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
#include <signal.h>
#include "RTS-types.h"
#include "rtpkt.h"
#include "tailor.h"

/*    RT-OPEN.REQUEST */

int	RtOpenRequest2 (mode, turn, context, callingtitle, calledtitle,
	callingaddr, calledaddr, ctxlist, defctxname, data, qos, tctx,
	rtc, rti)
int	mode,
	turn;
OID	context;
AEI	callingtitle,
	calledtitle;
struct PSAPaddr *callingaddr,
		*calledaddr;
struct PSAPctxlist *ctxlist;
OID	defctxname;
PE	data;
struct QOStype *qos;
OID	tctx;
struct RtSAPconnect *rtc;
struct RtSAPindication *rti;
{
    SBV	    smask;
	int result;

    isodetailor (NULLCP, 0);

    switch (mode) {
	case RTS_MONOLOGUE:
	case RTS_TWA:
	    break;

	default:
	    return rtsaplose (rti, RTS_PARAMETER, NULLCP,
		    "bad value for mode parameter");
    }
    switch (turn) {
	case RTS_INITIATOR:
	case RTS_RESPONDER:
	    break;

	default:
	    return rtsaplose (rti, RTS_PARAMETER, NULLCP,
		    "bad value for turn parameter");
    }
    if (!ctxlist) {
	static struct PSAPctxlist ctxs;

	ctxlist = &ctxs;
	bzero ((char *) ctxlist, sizeof *ctxlist);
    }
    missingP (rtc);
    bzero ((char *) rtc, sizeof *rtc);
    missingP (rti);

    smask = sigioblock ();

    result = RtOpenRequestAux (mode, turn, context, callingtitle, calledtitle,
	callingaddr, calledaddr, ctxlist, defctxname, data, qos, tctx,
	rtc, rti);

    (void) sigiomask (smask);

    return result;
}


/*  */

static int  RtOpenRequestAux (mode, turn, context, callingtitle, calledtitle, 
	callingaddr, calledaddr, ctxlist, defctxname, data, qos, tctx,
	rtc, rti)
int	mode,
	turn;
OID	context;
AEI	callingtitle,
	calledtitle;
struct PSAPaddr *callingaddr,
		*calledaddr;
struct PSAPctxlist *ctxlist;
OID	defctxname;
PE	data;
struct QOStype *qos;
OID	tctx;
struct RtSAPconnect *rtc;
struct RtSAPindication *rti;
{
    register int    i;
    int	    result,
	    requirements,
	    offset,
	    rtsid,
	    settings;
    PE	    pe,
	    p,
	    q;
    register struct assocblk *acb;
    struct SSAPref *sr;
    register struct PSAPcontext *pp;
    register struct AcSAPconnect *acc = &rtc -> rtc_connect;
    register struct PSAPconnect *pc = &acc -> acc_connect;
    struct AcSAPindication acis;
    register struct AcSAPindication *aci = &acis;
    register struct AcSAPabort *aca = &aci -> aci_abort;
    struct type_RTS_RTSE__apdus *rtpdu;
    struct type_RTS_RTOACapdu *prtoac;

/* begin RTORQ APDU */
    if ((pe = pe_alloc (PE_CLASS_CONT, PE_FORM_CONS, 16)) == NULLPE) {
no_mem: ;
	result = rtsaplose (rti, RTS_CONGEST, NULLCP, "out of memory");
	goto out1;
    }
    if ((DEFAULT_CKPOINT != PCONN_CK_DFLT
		&& set_add (pe, num2prim ((integer) DEFAULT_CKPOINT,
					  PE_CLASS_CONT, RTORQ_CKPOINT))
				== NOTOK)
	    || (DEFAULT_WINDOW != PCONN_WD_DFLT
			&& set_add (pe, num2prim ((integer) DEFAULT_WINDOW,
						  PE_CLASS_CONT, RTORQ_WINDOW))
				== NOTOK)
	    || set_add (pe, num2prim ((integer) (mode == RTS_TWA ? RTORQ_DM_TWA
			: RTORQ_DM_MONO), PE_CLASS_CONT, RTORQ_DIALOGUE))
		    == NOTOK
	    || set_add (pe, p = pe_alloc (PE_CLASS_CONT, PE_FORM_CONS,
		    RTORQ_CONNDATA)) == NOTOK
	    || set_add (p, q = pe_alloc (PE_CLASS_CONT, PE_FORM_CONS,
		    RTORQ_CD_OPEN)) == NOTOK
	    || set_add (q, data ? data : pe_alloc (PE_CLASS_CONT,
		    PE_FORM_PRIM, 0)) == NOTOK)
	goto no_mem;
/* end RTORQ APDU */

    requirements = RTS_MYREQUIRE;
    settings = 0;
#define dotoken(requires,shift,bit,type) \
{ \
    if (requirements & requires) \
	if (turn == RTS_INITIATOR) \
	    settings |= ST_INIT_VALUE << shift; \
	else \
	    settings |= ST_RESP_VALUE << shift; \
}
    dotokens ();
#undef	dotoken

    if ((sr = addr2ref (PLocalHostName ())) == NULL) {
	result = rtsaplose (rti, RTS_CONGEST, NULLCP, "out of memory");
	goto out1;
    }

    if (ctxlist -> pc_nctx >= NPCTX) {
	result = rtsaplose (rti, RTS_PARAMETER, NULLCP,
			    "too many contexts");
	goto out1;
    }
    {
	register int ctx;
	register OID oid;

	if (tctx)
		oid = tctx;	/* override standard context */
	else if ((oid = ode2oid (RT_ASN)) == NULLOID) {
	    result = rtsaplose (rti, RTS_PARAMETER, NULLCP,
				"%s: unknown", RT_ASN);
	    goto out1;
	}

	i = ctxlist -> pc_nctx - 1, ctx = 1;
	for (pp = ctxlist -> pc_ctx; i >= 0; i--, pp++) {
	    if (oid_cmp (pp -> pc_asn, oid) == 0) {
		rtsid = pp -> pc_id;
		offset = pp - ctxlist -> pc_ctx;

		pp = NULL;
		goto ready;
	    }
	    if (ctx <= pp -> pc_id)
		ctx = pp -> pc_id + 2;
	}
	pp -> pc_id = ctx;
	if ((pp -> pc_asn = oid_cpy (oid)) == NULLOID)
	    goto no_mem;
	pp -> pc_atn = NULLOID;

	rtsid = pp -> pc_id;
	offset = -1;

	ctxlist -> pc_nctx++;
    }
ready: ;
    pe -> pe_context = rtsid;

    PLOGP (rtsap_log,RTS_RTSE__apdus, pe, "RTORQapdu", 0);

    result = AcAssocRequest (context, callingtitle, calledtitle, callingaddr,
	    calledaddr, ctxlist, defctxname, 0, requirements,
	    SERIAL_NONE, settings, sr, &pe, 1, qos, acc, aci);

    if (pp) {
	oid_free (pp -> pc_asn);
	pp -> pc_asn = NULLOID;
    }

    if (data)
	(void) pe_extract (pe, data);
    pe_free (pe);
    pe = NULLPE;

    if (result == NOTOK) {
	(void) acs2rtslose (NULLACB, rti, "AcAssocRequest", aca);
	goto out1;
    }

    if (acc -> acc_result == ACS_ACCEPT) {
	if ((acb = findacblk (acc -> acc_sd)) == NULLACB) {
	    result = rtpktlose (NULLACB, rti, RTS_PROTOCOL, NULLCP, "ACSE mangled");
	    goto out2;
	}
    }
    else
	if (acc -> acc_result == ACS_ABORTED) {
	    (void) acs2rtsabort (acb = NULLACB, aca, rti);

	    rtc -> rtc_sd = NOTOK;
	    rtc -> rtc_result = RTS_ABORTED;

	    result = OK;
	    goto out2;
	}
	else
	    acb = NULLACB;

    if ((pe = acc -> acc_info[0]) == NULLPE) {
	if (acc -> acc_result != ACS_ACCEPT) {
	    aca -> aca_reason = acc -> acc_result;
	    (void) acs2rtslose (acb, rti, "AcAssocRequest(pseudo)", aca);

	    rtc -> rtc_sd = NOTOK;
	    rtc -> rtc_result = rti -> rti_abort.rta_reason;

	    result = OK;
	}
	else
	    if (acb)
		result = rtpktlose (acb, rti, RTS_PROTOCOL, NULLCP, NULLCP);
	    else
		result = rtsaplose (rti, RTS_PROTOCOL, NULLCP, NULLCP);
	goto out2;
    }

    if (acc -> acc_result != ACS_ACCEPT) {
	struct type_RTS_RTORJapdu *prtorj;

	if (decode_RTS_RTSE__apdus (pe, 1, NULLIP, NULLVP, &rtpdu) == NOTOK) {
	    result = pylose ();
	    goto out2;
	}

	PLOGP (rtsap_log,RTS_RTSE__apdus, pe, "RTORJapdu", 1);

	if (rtpdu -> offset != type_RTS_RTSE__apdus_rtorj__apdu) {
	    result = rtpktlose (acb, rti, RTS_PROTOCOL, NULLCP,
				"Unexpected PDU");
	    free_RTS_RTSE__apdus (rtpdu);
	    goto out2;
	}
	prtorj = rtpdu -> un.rtorj__apdu;

	rtc -> rtc_sd = NOTOK;
	rtc -> rtc_result = RTS_REJECT;
	(void) pe_extract (pe, rtc -> rtc_data = prtorj -> userDataRJ);
	if (rtc -> rtc_data)
	    rtc -> rtc_data -> pe_refcnt ++;

	for (i = acc -> acc_ninfo - 1; i >= 0; i--) 
	    if (acc -> acc_info[i]) {
		pe_free (acc -> acc_info[i]);
		acc -> acc_info[i] = NULLPE;
	    }
	acc -> acc_ninfo = 0;

	free_RTS_RTSE__apdus (rtpdu);
	return OK;
    }

    acb -> acb_flags |= ACB_RTS | ACB_INIT;
    acb -> acb_uabort = AcUAbortRequest;
    SetPS2RtService (acb);
    if (turn == RTS_INITIATOR)
	acb -> acb_flags |= ACB_TURN;
    if (mode == RTS_TWA)
	acb -> acb_flags |= ACB_TWA;
    acb -> acb_connect = *sr;	/* struct copy */
    if ((acb -> acb_requirements = pc -> pc_srequirements) != RTS_MYREQUIRE) {
	result = rtpktlose (acb, rti, RTS_PROTOCOL, NULLCP,
		    "desired session requirements denied");
	goto out2;
    }
#define dotoken(requires,shift,bit,type) \
{ \
    if (acb -> acb_requirements & requires) \
	switch (pc -> pc_settings & (ST_MASK << shift)) { \
	    case ST_INIT_VALUE << shift: \
		acb -> acb_owned |= bit; \
		acb -> acb_avail |= bit; \
		break; \
 \
	    case ST_RESP_VALUE << shift: \
		acb -> acb_avail |= bit; \
		break; \
 \
	    default: \
		result = rtpktlose (acb, rti, RTS_PROTOCOL, NULLCP, \
			    "%s token management botched", type); \
		goto out2; \
	} \
}
	dotokens ();
#undef	dotoken
    switch (turn) {
	case RTS_INITIATOR:
	    if (acb -> acb_owned == acb -> acb_avail)
		break;
	    result = rtsaplose (rti, RTS_PROTOCOL, NULLCP,
		    "token management botched");
	    goto out2;

	case RTS_RESPONDER:
	    if (acb -> acb_owned == 0)
		break;
	    result = rtsaplose (rti, RTS_PROTOCOL, NULLCP,
		    "token management botched");
	    goto out2;
    }
    acb -> acb_ssdusize = pc -> pc_ssdusize;

    if (decode_RTS_RTSE__apdus (pe, 1, NULLIP, NULLVP, &rtpdu) == NOTOK) {
	result = pylose ();
	goto out3;
    }

    PLOGP (rtsap_log,RTS_RTSE__apdus, pe, "RTOACapdu", 1);

    if (rtpdu -> offset != type_RTS_RTSE__apdus_rtoac__apdu) {
	result = rtpktlose (acb, rti, RTS_PROTOCOL, NULLCP,
			    "Unexpected PDU");
	goto out3;
    }

    prtoac = rtpdu -> un.rtoac__apdu;
    acb -> acb_ckpoint = prtoac->checkpointSize;
    acb -> acb_window = prtoac->windowSize;

    if ((i = offset) < 0)
	i = pc -> pc_ctxlist.pc_nctx - 1;
    pp = pc -> pc_ctxlist.pc_ctx + i;
    if (pp -> pc_id != rtsid) {
	result = rtpktlose (acb, rti, RTS_PROTOCOL, NULLCP,
			    "RTSE PCI not found");
	goto out3;
    }
    if (pp -> pc_result != PC_ACCEPT) {
	result = rtpktlose (acb, rti, RTS_PROTOCOL, NULLCP,
			    "RTSE PCI rejected");
	goto out3;
    }

    if (offset < 0)
	pc -> pc_ctxlist.pc_nctx--;
    acb -> acb_rtsid = rtsid;
	
    rtc -> rtc_sd = acb -> acb_fd;
    rtc -> rtc_result = RTS_ACCEPT;
    if (prtoac -> connectionDataAC -> offset == type_RTS_ConnectionData_open) {
	rtc -> rtc_data = prtoac -> connectionDataAC -> un.open;
	if (rtc -> rtc_data)
	    rtc -> rtc_data -> pe_refcnt ++;
    }
    else
	rtc -> rtc_data = NULL;
    (void) pe_extract (pe, rtc -> rtc_data);

    for (i = acc -> acc_ninfo - 1; i >= 0; i--) 
	if (acc -> acc_info[i]) {
	    pe_free (acc -> acc_info[i]);
	    acc -> acc_info[i] = NULLPE;
	}
    acc -> acc_ninfo = 0;

    free_RTS_RTSE__apdus (rtpdu);
    if (tctx) oid_free (tctx);
    return OK;

out3:
    free_RTS_RTSE__apdus (rtpdu);
out2: ;
    ACCFREE (acc);
    if (acb)
	freeacblk (acb);
	
out1: ;
    if (tctx) oid_free (tctx);
    if (pe) {
	if (data)
	    (void) pe_extract (pe, data);
	pe_free (pe);
    }

    return result;
}

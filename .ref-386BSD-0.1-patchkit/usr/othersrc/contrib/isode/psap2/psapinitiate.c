/* psapinitiate.c - PPM: initiator */

#ifndef	lint
static char *rcsid = "$Header: /f/osi/psap2/RCS/psapinitiate.c,v 7.4 91/02/22 09:37:31 mrose Interim $";
#endif

/* 
 * $Header: /f/osi/psap2/RCS/psapinitiate.c,v 7.4 91/02/22 09:37:31 mrose Interim $
 *
 *
 * $Log:	psapinitiate.c,v $
 * Revision 7.4  91/02/22  09:37:31  mrose
 * Interim 6.8
 * 
 * Revision 7.3  90/07/01  21:05:00  mrose
 * pepsy
 * 
 * Revision 7.2  90/01/27  10:26:29  mrose
 * touch-up
 * 
 * Revision 7.1  89/11/24  16:22:14  mrose
 * sync
 * 
 * Revision 7.0  89/11/23  22:14:20  mrose
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
#include "PS-types.h"
#include "ppkt.h"
#include "isoservent.h"
#include "tailor.h"

/*    P-(ASYN-)CONNECT.REQUEST */

int	PAsynConnRequest (calling, called, ctxlist, defctxname, prequirements,
	srequirements, isn, settings, ref, data, ndata, qos, pc, pi, async)
struct PSAPaddr *calling,
		*called;
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
struct PSAPconnect *pc;
struct PSAPindication *pi;
{
    SBV     smask;
    int     result;

    isodetailor (NULLCP, 0);

#ifdef	notdef
    missingP (calling);
#endif
    missingP (called);

    if (ctxlist && ctxlist -> pc_nctx > NPCTX)
	return psaplose (pi, PC_PARAMETER, NULLCP,
		"only %d proposed presentation contexts supported", NPCTX);
    if (prequirements & ~PR_MYREQUIRE)
	return psaplose (pi, PC_PARAMETER, NULLCP,
		"presentation requirements settings not supported");
    if ((prequirements & PR_RESTORATION)
	    && !(prequirements & PR_MANAGEMENT))
	return psaplose (pi, PC_PARAMETER, NULLCP,
	    "context restoration service requires context management service");

/* let session provider catch errors in session parameters */

    toomuchP (data, ndata, NPDATA, "initial");
    missingP (pc);
    missingP (pi);

    smask = sigioblock ();

    result = PConnRequestAux (calling, called, ctxlist, defctxname,
	    prequirements, srequirements, isn, settings, ref, data, ndata,
	    qos, pc, pi, async);

    (void) sigiomask (smask);

    return result;
}

/*  */

static int  PConnRequestAux (calling, called, ctxlist, defctxname,
	prequirements, srequirements, isn, settings, ref, data, ndata, qos,
	pc, pi, async)
struct PSAPaddr *calling,
		*called;
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
struct PSAPconnect *pc;
struct PSAPindication *pi;
{
    int	    i,
	    len,
	    result;
    PE	    pe;
    register struct psapblk *pb;
    struct SSAPconnect scs;
    register struct SSAPconnect *sc = &scs;
    struct SSAPindication sis;
    register struct SSAPindication *si = &sis;
    register struct SSAPabort *sa = &si -> si_abort;
    register struct type_PS_CP__type *pdu;
    register struct element_PS_0 *normal;

    if ((pb = newpblk ()) == NULL)
	return psaplose (pi, PC_CONGEST, NULLCP, "out of memory");

    pb -> pb_srequirements = pb -> pb_urequirements = srequirements;

#ifdef	notdef
    if (called -> pa_selectlen > 0) {
	if (calling == NULLPA) {
	    static struct PSAPaddr pas;

	    calling = &pas;
	    bzero ((char *) calling, sizeof *calling);
	}

	if (calling -> pa_selectlen == 0) {
	    calling -> pa_port =
			    htons ((u_short) (0x8000 | (getpid () & 0x7fff)));
	    calling -> pa_selectlen = sizeof calling -> pa_port;
	}
    }
#endif

    pe = NULLPE;
    if ((pdu = (struct type_PS_CP__type *) calloc (1, sizeof *pdu)) == NULL) {
no_mem: ;
	(void) psaplose (pi, PC_CONGEST, NULLCP, "out of memory");
	goto out2;
    }
    if ((pdu -> mode = (struct type_PS_Mode__selector *)
			    malloc (sizeof *pdu -> mode)) == NULL)
	goto no_mem;
    pdu -> mode -> parm = int_PS_Mode__selector_normal__mode;
    if ((pdu -> normal__mode = (struct element_PS_0 *)
				    calloc (1, sizeof *pdu -> normal__mode))
	    == NULL)
	goto no_mem;
    normal = pdu -> normal__mode;
    if (calling
	    && calling -> pa_selectlen > 0
	    && (normal -> calling = str2qb (calling -> pa_selector,
					    calling -> pa_selectlen, 1))
		    == NULL)
	goto no_mem;
    if (called -> pa_selectlen > 0
	    && (normal -> called = str2qb (called -> pa_selector,
					    called -> pa_selectlen, 1))
		    == NULL)
	goto no_mem;

    if ((pb -> pb_asn = ode2oid (DFLT_ASN)) == NULLOID)  {
	(void) psaplose (pi, PC_ABSTRACT, NULLCP, "%s: unknown", DFLT_ASN);
	goto out2;
    }
    if ((pb -> pb_asn = oid_cpy (pb -> pb_asn)) == NULLOID)
	goto no_mem;
    if ((pb -> pb_atn = ode2oid (DFLT_ATN)) == NULLOID)  {
	(void) psaplose (pi, PC_TRANSFER, NULLCP, "%s: unknown", DFLT_ATN);
	goto out2;
    }
    if ((pb -> pb_atn = oid_cpy (pb -> pb_atn)) == NULLOID)
	goto no_mem;
    if ((pb -> pb_ber = oid_cpy (pb -> pb_atn)) == NULLOID)
	goto no_mem;

    if (ctxlist && ctxlist -> pc_nctx > 0) {
	register struct type_PS_Definition__list *cd,
						**cp;
	register struct PSAPcontext *pp,
				    *qp;

	cp = &normal -> context__list;

	i = ctxlist -> pc_nctx - 1;
	for (pp = ctxlist -> pc_ctx, qp = pb -> pb_contexts;
		i >= 0;
		i--, pp++, qp++){
	    if (!((qp -> pc_id = pp -> pc_id) & 01)) {
		(void) psaplose (pi, PC_PARAMETER, NULLCP,
			    "only odd values allowed for context identifiers");
		goto out2;
	    }

	    if (pp -> pc_asn == NULLOID) {
		(void) psaplose (pi, PC_PARAMETER, NULLCP,
			    "no abstract syntax name given for context %d",
			    pp -> pc_id);
		goto out2;
	    }
	    if ((qp -> pc_asn = oid_cpy (pp -> pc_asn)) == NULLOID)
		goto no_mem;

	    if (pp -> pc_atn && !atn_is_ok (pb, pp -> pc_atn)) {
		(void) psaplose (pi, PC_TRANSFER, NULLCP,
			    "unknown transfer syntax name given for context %d",
			    pp -> pc_id);
		goto out2;
	    }
	    if ((qp -> pc_atn = oid_cpy (pp -> pc_atn ? pp -> pc_atn
			: pb -> pb_atn)) == NULLOID)
		goto no_mem;

	    if ((cd =(struct type_PS_Definition__list *)
			calloc (1, sizeof *cd)) == NULL)
		goto no_mem;
	    *cp = cd;
	    cp = &cd -> next;
	    if ((cd -> element_PS_5 = (struct element_PS_6 *)
			    calloc (1, sizeof *cd -> element_PS_5)) == NULL)
		goto no_mem;
	    cd -> element_PS_5 -> identifier = qp -> pc_id;
	    if ((cd -> element_PS_5 -> abstract__syntax =
				oid_cpy (qp -> pc_asn)) == NULLOID
		    || (cd -> element_PS_5 -> transfer__syntax__list =
			    (struct element_PS_7 *)
				    calloc (1, sizeof (struct element_PS_7)))
			    == NULL
		    || (cd -> element_PS_5 -> transfer__syntax__list ->
			        Transfer__syntax__name =
					oid_cpy (qp -> pc_atn))
			    == NULL)
		goto no_mem;

	    qp -> pc_result = PC_ACCEPT;

	    pb -> pb_ncontext++;
	}
    }

    if (defctxname) {
	oid_free (pb -> pb_asn);
	if ((pb -> pb_asn = oid_cpy (defctxname)) == NULLOID
		|| (normal -> default__context =
			(struct type_PS_Context__name *)
			    calloc (1, sizeof *normal -> default__context))
			== NULL
	        || (normal -> default__context -> abstract__syntax =
			    oid_cpy (pb -> pb_asn)) == NULLOID
				/* perhaps should be user-definable */
	        || (normal -> default__context -> transfer__syntax =
			    oid_cpy (pb -> pb_atn)) == NULLOID)
	    goto no_mem;
    }
    pb -> pb_result = PC_ACCEPT;

    if ((pb -> pb_prequirements = prequirements) != PR_MYREQUIRE) {
	register struct pair *pp;

	if ((normal -> presentation__fu = prim2bit (pe_alloc (PE_CLASS_UNIV,
							      PE_FORM_PRIM,
							      PE_PRIM_BITS)))
	         == NULL)
	    goto no_mem;

	for (pp = preq_pairs; pp -> p_mask; pp++)
	    if ((pb -> pb_prequirements & pp -> p_mask)
		    && bit_on (normal -> presentation__fu, pp -> p_bitno)
			    == NOTOK)
	    goto no_mem;

       if (bit2prim (normal -> presentation__fu) == NULLPE)
	   goto no_mem;
    }

    if (pb -> pb_prequirements & PR_MANAGEMENT)
	pb -> pb_srequirements |= SR_TYPEDATA;
    if (pb -> pb_urequirements != pb -> pb_srequirements) {
	register struct pair *pp;

	if ((normal -> session__fu = prim2bit (pe_alloc (PE_CLASS_UNIV,
							 PE_FORM_PRIM,
							 PE_PRIM_BITS)))
	        == NULL)
	    goto no_mem;

	for (pp = sreq_pairs; pp -> p_mask; pp++)
	    if ((pb -> pb_urequirements & pp -> p_mask)
		    && bit_on (normal -> session__fu, pp -> p_bitno) == NOTOK)
		goto no_mem;

	if (bit2prim (normal -> session__fu) == NULLPE)
	    goto no_mem;
    }

    if (data
	    && ndata > 0
	    && (normal -> user__data = info2ppdu (pb, pi, data, ndata,
						  PPDU_CP)) == NULL)
	goto out2;   

    if (encode_PS_CP__type (&pe, 1, 0, NULLCP, pdu) == NOTOK) {
	(void) psaplose (pi, PC_CONGEST, NULLCP, "error encoding PDU: %s",
			 PY_pepy);
	goto out2;
    }

    PLOGP (psap2_log,PS_CP__type, pe, "CP-type", 0);

    if (pe2ssdu (pe, &pb -> pb_retry, &len) == NOTOK)
	goto no_mem;

    free_PS_CP__type (pdu);
    pdu = NULL;

    pe_free (pe);
    pe = NULLPE;

    if ((result = SAsynConnRequest (ref, calling ? &calling -> pa_addr
	    : NULLSA, &called -> pa_addr, pb -> pb_srequirements, settings,
	    isn, pb -> pb_retry, len, qos, sc, si, async)) == NOTOK) {
	(void) ss2pslose (NULLPB, pi, "SAsynConnRequest", sa);
	goto out1;
    }

    pb -> pb_fd = sc -> sc_sd;

    if (async) {
	switch (result) {
	    case CONNECTING_1:
	    case CONNECTING_2:
	    pc -> pc_sd = pb -> pb_fd;
	    return result;
	}
    }
    if ((result = PAsynRetryAux (pb, sc, si, pc, pi)) == DONE && !async)
	result = OK;
    return result;

out2: ;
    if (pe)
	pe_free (pe);
    if (pdu)
	free_PS_CP__type (pdu);

out1: ;
    freepblk (pb);

    return NOTOK;
}

/*    P-ASYN-RETRY.REQUEST (pseudo) */

int	PAsynRetryRequest (sd, pc, pi)
int	sd;
struct PSAPconnect *pc;
struct PSAPindication *pi;
{
    SBV     smask;
    int     result;
    register struct psapblk *pb;
    struct SSAPconnect  scs;
    register struct SSAPconnect *sc = &scs;
    struct SSAPindication sis;
    register struct SSAPindication *si = &sis;
    register struct SSAPabort *sa = &si -> si_abort;

    missingP (pc);
    missingP (pi);

    smask = sigioblock ();

    if ((pb = findpblk (sd)) == NULL) {
	(void) sigiomask (smask);
	return psaplose (pi, PC_PARAMETER, NULLCP,
		"invalid presentation descriptor");
    }
    if (pb -> pb_flags & PB_CONN) {
	(void) sigiomask (smask);
	return psaplose (pi, PC_OPERATION, NULLCP,
		"presentation descriptor connected");
    }

    switch (result = SAsynRetryRequest (pb -> pb_fd, sc, si)) {
	case NOTOK: 
	    pb -> pb_fd = NOTOK;
	    (void) ss2pslose (pb, pi, "SAsynRetryRequest", sa);
	    freepblk (pb);
	    break;

	case CONNECTING_1:
	case CONNECTING_2:
	    break;

	case DONE: 
	    result = PAsynRetryAux (pb, sc, si, pc, pi);
	    break;
    }

    (void) sigiomask (smask);

    return result;
}

/*  */

static int  PAsynRetryAux (pb, sc, si, pc, pi)
register struct psapblk *pb;
struct SSAPconnect *sc;
struct SSAPindication *si;
struct PSAPconnect *pc;
struct PSAPindication *pi;
{
    int	    i,
	    result;
    PE	    pe;
    struct qbuf *qb;
    register struct SSAPabort *sa = &si -> si_abort;
    struct type_PS_CPA__type *cpa;
    register struct element_PS_1 *cpa_normal;
    struct type_PS_CPR__type *cpr;
    register struct element_PS_2 *cpr_normal;
    struct type_PS_ARP__PPDU *arp;

    pe = NULLPE;

    free (pb -> pb_retry);
    pb -> pb_retry = NULL;

    bzero ((char *) pc, sizeof *pc);

    if (sc -> sc_result == SC_ABORT) {
	(void) ss2psabort (pb, sa, pi);

	pc -> pc_sd = NOTOK;
	pc -> pc_result = PC_ABORTED;

	return DONE;
    }

    cpa = NULL, cpr = NULL, arp = NULL;
    if ((pe = ssdu2pe (sc -> sc_data, sc -> sc_cc, NULLCP, &result))
	    == NULLPE) {
	if (sc -> sc_result != SC_ACCEPT) {
	    bzero ((char *) sa, sizeof *sa);
	    sa -> sa_reason = sc -> sc_result;
	    pb -> pb_fd = NOTOK;
	    (void) ss2pslose (pb, pi, "SAsynConnRequest(pseudo)", sa);
	    
	    pc -> pc_sd = NOTOK;
	    pc -> pc_result = pi -> pi_abort.pa_reason;

	    result = DONE;
	    goto out1;
	}
	else
	    (void) ppktlose (pb, pi, result != PS_ERR_NMEM ? PC_UNRECOGNIZED
		    : PC_NOTSPECIFIED, sc -> sc_result == SC_ACCEPT ? PPDU_CPA
		    : PPDU_CPR, NULLCP, "%s", ps_error (result));
	goto out2;
    }

    SCFREE (sc);

    if (sc -> sc_result == SC_ACCEPT) {
	pb -> pb_flags |= PB_CONN;

	pb -> pb_srequirements = sc -> sc_requirements;
	pb -> pb_urequirements &= pb -> pb_srequirements;

#define dotoken(requires,shift,bit,type) \
{ \
	if (pb -> pb_srequirements & requires) \
	    switch (sc -> sc_settings & (ST_MASK << shift)) { \
		case ST_INIT_VALUE << shift: \
		    pb -> pb_owned |= bit; \
		    pb -> pb_avail |= bit; \
		    break; \
 \
		case ST_RESP_VALUE << shift: \
		    pb -> pb_avail |= bit; \
		    break; \
	    } \
}
	dotokens ();
#undef	dotoken

	pb -> pb_ssdusize = sc -> sc_ssdusize;

	if (decode_PS_CPA__type (pe, 1, NULLIP, NULLVP, &cpa) == NOTOK) {
	    (void) ppktlose (pb, pi, PC_UNRECOGNIZED, PPDU_CPA, NULLCP, "%s",
			     PY_pepy);
	    goto out2;
	}

	PLOGP (psap2_log,PS_CPA__type, pe, "CPA-type", 1);

	if (cpa -> mode -> parm != int_PS_Mode__selector_normal__mode) {
	    (void) ppktlose (pb, pi, PC_INVALID, PPDU_CPA, NULLCP,
			"X.410 mode mismatch");
	    goto out2;
	}
	cpa_normal = cpa -> normal__mode;

	pc -> pc_sd = pb -> pb_fd;
    }
    else {
	if (sc -> sc_result == SC_NOTSPECIFIED) {
	    if (decode_PS_ARP__PPDU (pe, 1, NULLIP, NULLVP, &arp) == NOTOK) {
		(void) ppktlose (pb, pi, PC_UNRECOGNIZED, PPDU_ARP, NULLCP,
				 "%s", PY_pepy);
		goto out2;
	    }

	    PLOGP (psap2_log,PS_ARP__PPDU, pe, "ARP-PPDU", 1);

	    if (arp -> provider__reason) {
		if ((result = arp -> provider__reason -> parm) == 0)
		    result = PC_NOTSPECIFIED;
		else
		    result += PC_ABORT_BASE;
	    }
	    else
		result = PC_NOTSPECIFIED;

	    (void) psaplose (pi, result, NULLCP, NULLCP);
	    goto out2;
	}

	if (decode_PS_CPR__type (pe, 1, NULLIP, NULLVP, &cpr) == NOTOK) {
	    (void) ppktlose (pb, pi, PC_UNRECOGNIZED, PPDU_CPR, NULLCP, "%s",
			     PY_pepy);
	    goto out2;
	}

	PLOGP (psap2_log,PS_CPR__type, pe, "CPR-type", 1);

	if (cpr -> offset != type_PS_CPR__type_normal__mode) {
	    (void) ppktlose (pb, pi, PC_INVALID, PPDU_CPR, NULLCP,
			"X.410 mode mismatch");
	    goto out2;
	}
	cpr_normal = cpr -> un.normal__mode;

	pc -> pc_sd = NOTOK;
    }

    pb -> pb_responding.pa_addr = sc -> sc_responding;	/* struct copy */
    if (qb = cpa ? cpa_normal -> responding : cpr_normal -> responding) {
	char   *base;

	if ((base = qb2str (qb)) == NULLCP
	        && sc -> sc_result == SC_ACCEPT) {
	    (void) ppktlose (pb, pi, PC_INVALID, PPDU_CPA, NULLCP,
			"malformed PSAP selector");
	    goto out2;
	}
	if (base) {
	    if (qb -> qb_len > sizeof pc -> pc_responding.pa_selector)
		qb -> qb_len = sizeof pc -> pc_responding.pa_selector;
	    bcopy (base, pb -> pb_responding.pa_selector,
			pb -> pb_responding.pa_selectlen = qb -> qb_len);
	    free (base);
	}
    }
    pc -> pc_responding = pb -> pb_responding;	/* struct copy */

    {
	register struct PSAPcontext *pp,
				    *qp;
	register struct type_PS_Definition__result__list *lp,
							 *mp;

	i = 0;
	lp = cpa ? cpa_normal -> context__list : cpr_normal -> context__list;
	for (mp = lp; mp; mp = mp -> next)
	    i++;
	if (i != pb -> pb_ncontext && i != 0) {
	    if (sc -> sc_result != SC_ACCEPT) {
		pc -> pc_ctxlist.pc_nctx = 0;
		goto carry_on;
	    }
	
	    (void) ppktlose (pb, pi, PC_INVALID, PPDU_CPA, NULLCP,
			"proposed/resulting presentation contexts mismatch");
	    goto out2;
	}

	i = (pc -> pc_ctxlist.pc_nctx = pb -> pb_ncontext) - 1;
	for (pp = pc -> pc_ctxlist.pc_ctx, qp = pb -> pb_contexts, mp = lp;
		i >= 0;
		i--, pp++, qp++) {
	    pp -> pc_id = qp -> pc_id;
	    pp -> pc_asn = pp -> pc_atn = NULLOID;

	    if (lp == NULL) {
		pp -> pc_result = PC_ACCEPT;
		continue;
	    }

	    switch (mp -> element_PS_12 -> result) {
		case int_PS_result_acceptance:
					    /* assume they gave back ASN.1 */
		default:
		    pp -> pc_result = qp -> pc_result = PC_ACCEPT;
		    break;

		case int_PS_result_user__rejection:
		    pp -> pc_result = qp -> pc_result = PC_REJECTED;
		    break;

		case int_PS_result_provider__rejection:
		    if ((pp -> pc_result =
			     mp -> element_PS_12 -> provider__reason) == 0) 
			pp -> pc_result = PC_NOTSPECIFIED;
		    else
			pp -> pc_result += PC_REASON_BASE;
		    qp -> pc_result = pp -> pc_result;
		    break;
	    }

	    mp = mp -> next;
	}

	i = pb -> pb_ncontext - 1;
	for (qp = pb -> pb_contexts + i; i >= 0; i--, qp--)
	    if (qp -> pc_result != PC_ACCEPT) {
		register struct PSAPcontext *qqp;

		qqp = pb -> pb_contexts + --pb -> pb_ncontext;
		if (qp -> pc_asn) {
		    oid_free (qp -> pc_asn);
		    qp -> pc_asn = NULLOID;
		}
		if (qp -> pc_atn) {
		    oid_free (qp -> pc_atn);
		    qp -> pc_atn = NULLOID;
	        }
		if (qp != qqp)
		    *qp = *qqp;	/* struct copy */
	    }
    }
carry_on: ;

    if (cpr == NULL || cpr_normal -> default__context == NULL)
	pc -> pc_defctxresult = PC_ACCEPT;
    else
        switch (cpr_normal -> default__context -> parm) {
	    case int_PS_Result_acceptance:
	    default:
	        pc -> pc_defctxresult = PC_ACCEPT;
		break;
		
	    case int_PS_Result_user__rejection:
		pc -> pc_defctxresult = PC_REJECTED;
		break;
		
	    case int_PS_Result_provider__rejection:
		pc -> pc_defctxresult = PC_NOTSPECIFIED;
		break;
	}

    if (ppdu2info (pb, pi, cpa ? cpa_normal -> user__data
			       : cpr_normal -> user__data, pc -> pc_info,
		   &pc -> pc_ninfo, cpa ? PPDU_CPA : PPDU_CPR) == NOTOK)
	goto out2;

    if (sc -> sc_result == SC_ACCEPT) {
	if (cpa_normal -> presentation__fu) {
	    register struct pair *pp;

	    if (!(pb -> pb_srequirements & SR_TYPEDATA)) {
		(void) bit_off (cpa_normal -> presentation__fu,
			bit_PS_Presentation__requirements_context__management);
		(void) bit_off (cpa_normal -> presentation__fu,
				bit_PS_Presentation__requirements_restoration);
	    }
	    for (pp = preq_pairs; pp -> p_mask; pp++)
		if (!(pb -> pb_prequirements & pp -> p_mask)) {
		    if (bit_test (cpa_normal -> presentation__fu,
				  pp -> p_bitno) == 1) {
			(void) ppktlose (pb, pi, PC_INVALID, PPDU_CPA, NULLCP,
			      "presentation-requirements negotiation botched");
			goto out2;
		    }
		}
		else
		    if (bit_test (cpa_normal -> presentation__fu,
				  pp -> p_bitno) < 1)
			pb -> pb_prequirements &= ~pp -> p_mask;
	}
	pc -> pc_prequirements = pb -> pb_prequirements;

	if (cpa_normal -> session__fu) {
	    register struct pair *pp;

	    for (pp = preq_pairs; pp -> p_mask; pp++)
		if (bit_test (cpa_normal -> session__fu, pp -> p_bitno) < 1)
		    pb -> pb_urequirements &= ~pp -> p_mask;
	}
	pc -> pc_srequirements = pb -> pb_urequirements;
	pc -> pc_settings = sc -> sc_settings;
	pc -> pc_please = sc -> sc_please;
	pc -> pc_isn = sc -> sc_isn;

	pc -> pc_connect = sc -> sc_connect;	/* struct copy */

	pc -> pc_ssdusize = sc -> sc_ssdusize;

	pc -> pc_qos = sc -> sc_qos;	/* struct copy */

	pc -> pc_result = PC_ACCEPT;

	free_PS_CPA__type (cpa);
    }
    else {
	if (cpr_normal -> optionals & opt_PS_element_PS_2_reason)
	    pc -> pc_result = cpr_normal -> reason + PC_PROV_BASE;
	else
	    pc -> pc_result = PC_NOTSPECIFIED;

	free_PS_CPR__type (cpr);
	freepblk (pb);
    }

    pe_free (pe);

    return DONE;

out2: ;
    result = NOTOK;
    if (pe)
	pe_free (pe);
    if (cpa)
	free_PS_CPA__type (cpa);
    if (cpr)
	free_PS_CPR__type (cpr);
    if (arp)
	free_PS_ARP__PPDU (arp);

out1: ;
    SCFREE (sc);
    freepblk (pb);

    return result;
}

/*    P-ASYN-NEXT.REQUEST (pseudo) */

int	PAsynNextRequest (sd, pc, pi)
int	sd;
struct PSAPconnect *pc;
struct PSAPindication *pi;
{
    SBV     smask;
    int     result;
    register struct psapblk *pb;
    struct SSAPconnect  scs;
    register struct SSAPconnect *sc = &scs;
    struct SSAPindication sis;
    register struct SSAPindication *si = &sis;
    register struct SSAPabort *sa = &si -> si_abort;

    missingP (pc);
    missingP (pi);

    smask = sigioblock ();

    if ((pb = findpblk (sd)) == NULL) {
	(void) sigiomask (smask);
	return psaplose (pi, PC_PARAMETER, NULLCP,
		"invalid presentation descriptor");
    }
    if (pb -> pb_flags & PB_CONN) {
	(void) sigiomask (smask);
	return psaplose (pi, PC_OPERATION, NULLCP,
		"presentation descriptor connected");
    }

    switch (result = SAsynNextRequest (pb -> pb_fd, sc, si)) {
	case NOTOK: 
	    pb -> pb_fd = NOTOK;
	    (void) ss2pslose (pb, pi, "SAsynRetryRequest", sa);
	    freepblk (pb);
	    break;

	case CONNECTING_1:
	case CONNECTING_2:
	    break;

	case DONE: 
	    result = PAsynRetryAux (pb, sc, si, pc, pi);
	    break;
    }

    (void) sigiomask (smask);

    return result;
}

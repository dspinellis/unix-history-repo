/* psaprespond.c - PPM: responder */

#ifndef	lint
static char *rcsid = "$Header: /f/osi/psap2/RCS/psaprespond.c,v 7.6 91/03/09 11:55:40 mrose Exp $";
#endif

/* 
 * $Header: /f/osi/psap2/RCS/psaprespond.c,v 7.6 91/03/09 11:55:40 mrose Exp $
 *
 *
 * $Log:	psaprespond.c,v $
 * Revision 7.6  91/03/09  11:55:40  mrose
 * update
 * 
 * Revision 7.5  91/02/22  09:37:44  mrose
 * Interim 6.8
 * 
 * Revision 7.4  90/10/23  20:43:51  mrose
 * update
 * 
 * Revision 7.3  90/10/17  11:52:49  mrose
 * sync
 * 
 * Revision 7.2  90/07/01  21:05:08  mrose
 * pepsy
 * 
 * Revision 7.1  89/11/24  16:22:17  mrose
 * sync
 * 
 * Revision 7.0  89/11/23  22:14:30  mrose
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
#include "PS-types.h"
#include "ppkt.h"
#include "tailor.h"

/*    P-CONNECT.INDICATION */

int	PInit (vecp, vec, ps, pi)
int	vecp;
char  **vec;
struct PSAPstart *ps;
struct PSAPindication *pi;
{
    int	    i,
	    len,
	    result,
	    result2;
    char   *base;
    register struct psapblk *pb;
    PE	    pe = NULLPE;
    struct SSAPref ref;
    struct SSAPstart    sss;
    register struct SSAPstart *ss = &sss;
    struct SSAPindication   sis;
    register struct SSAPindication *si = &sis;
    register struct SSAPabort  *sa = &si -> si_abort;
    struct type_PS_CP__type *cp;
    register struct element_PS_0 *cp_normal;
    register struct type_PS_CPR__type *cpr;

    isodetailor (NULLCP, 0);

    missingP (vec);
    missingP (ps);
    missingP (pi);

    if ((pb = newpblk ()) == NULL)
	return psaplose (pi, PC_CONGEST, NULLCP, "out of memory");

    cp = NULL, cpr = NULL;

    if (SInit (vecp, vec, ss, si) == NOTOK) {
	(void) ss2pslose (pb, pi, "SInit", sa);
	goto out2;
    }

    pb -> pb_fd = ss -> ss_sd;

    if ((pe = ssdu2pe (ss -> ss_data, ss -> ss_cc, NULLCP, &result))
	    == NULLPE) {
	if (result == PS_ERR_NMEM)
	    goto congest;

	(void) ppktlose (pb, pi, PC_PROTOCOL, PPDU_CP, NULLCP, "%s",
			 ps_error (result));
	goto out1;
    }

    SSFREE (ss);

    pb -> pb_srequirements = ss -> ss_requirements;

    pb -> pb_ssdusize = ss -> ss_ssdusize;

    bzero ((char *) ps, sizeof *ps);

    if (decode_PS_CP__type (pe, 1, NULLIP, NULLVP, &cp) == NOTOK) {
	(void) ppktlose (pb, pi, PC_UNRECOGNIZED, PPDU_CP, NULLCP, "%s",
			 PY_pepy);
	goto out1;
    }

    PLOGP (psap2_log,PS_CP__type, pe, "CP-type", 1);

    if (cp -> mode -> parm != int_PS_Mode__selector_normal__mode) {
	(void) ppktlose (pb, pi, PC_INVALID, PPDU_CP, NULLCP,
			 "X.410-mode not supported");
	goto out1;
    }
    cp_normal = cp -> normal__mode;

    ps -> ps_sd = pb -> pb_fd;

    ps -> ps_connect = ss -> ss_connect;	/* struct copy */

    ps -> ps_calling.pa_addr = ss -> ss_calling;/* struct copy */
    if (cp_normal -> calling) {
	if ((base = qb2str (cp_normal -> calling)) == NULLCP)
	    goto congest;
	if ((len = cp_normal -> calling -> qb_len) >
	         sizeof ps -> ps_calling.pa_selector)
	    len = sizeof ps -> ps_calling.pa_selector;
	bcopy (base, ps -> ps_calling.pa_selector,
		ps -> ps_calling.pa_selectlen = len);
	free (base);
    }
    ps -> ps_called.pa_addr = ss -> ss_called;	/* struct copy */
    if (cp_normal -> called) {
	if ((base = qb2str (cp_normal -> called)) == NULLCP)
	    goto congest;
	if ((len = cp_normal -> called -> qb_len) >
	         sizeof ps -> ps_called.pa_selector)
	    len = sizeof ps -> ps_called.pa_selector;
	bcopy (base, ps -> ps_called.pa_selector,
		ps -> ps_called.pa_selectlen = len);
	free (base);
    }

    if ((pb -> pb_asn = ode2oid (DFLT_ASN)) == NULLOID)  {
	(void) ppktlose (pb, pi, PC_ABSTRACT, PPDU_CP, NULLCP, "%s: unknown",
		    DFLT_ASN);
	goto out1;
    }
    if ((pb -> pb_asn = oid_cpy (pb -> pb_asn)) == NULLOID)
	goto congest;
    if ((pb -> pb_atn = ode2oid (DFLT_ATN)) == NULLOID)  {
	(void) ppktlose (pb, pi, PC_TRANSFER, PPDU_CP, NULLCP, "%s: unknown",
		    DFLT_ATN);
	goto out1;
    }
    if ((pb -> pb_atn = oid_cpy (pb -> pb_atn)) == NULLOID)
	goto congest;
    if ((pb -> pb_ber = oid_cpy (pb -> pb_atn)) == NULLOID)
	goto congest;

    {
	register struct PSAPcontext *pp,
				    *qp;
	register struct type_PS_Definition__list *lp;

	i = 0;
	for (lp = cp_normal -> context__list,
		    pp = ps -> ps_ctxlist.pc_ctx,
		    qp = pb -> pb_contexts;
	         lp;
	         lp = lp -> next, pp++, qp++, i++) {
	    register struct element_PS_6 *pctx = lp -> element_PS_5;
	    register struct element_PS_7 *atn;

	    pp -> pc_id = qp -> pc_id = pctx -> identifier;

	    pp -> pc_result = PC_ACCEPT;
	    pp -> pc_asn = pctx -> abstract__syntax;
	    pctx -> abstract__syntax = NULLOID;
	    if ((qp -> pc_asn = oid_cpy (pp -> pc_asn)) == NULLOID)
		goto congest;

	    for (atn = pctx -> transfer__syntax__list; atn; atn = atn -> next)
		if (atn_is_ok (pb, atn -> Transfer__syntax__name)) {
		    qp -> pc_atn = atn -> Transfer__syntax__name;
		    atn -> Transfer__syntax__name = NULLOID;
		    break;
		}
	    if (atn == NULL)
		pp -> pc_result = PC_TRANSFER;
	    pp -> pc_atn = NULLOID;

	    qp -> pc_result = pp -> pc_result;
	}

	ps -> ps_ctxlist.pc_nctx = pb -> pb_ncontext = i;
    }

    if (ppdu2info (pb, pi, cp_normal -> user__data, ps -> ps_info,
		   &ps -> ps_ninfo, PPDU_CP) == NOTOK)
	goto out1;
    
    ps -> ps_defctxresult = PC_ACCEPT;
    if (cp_normal -> default__context) {
	oid_free (pb -> pb_asn);
	pb -> pb_asn = cp_normal -> default__context -> abstract__syntax;
	cp_normal -> default__context -> abstract__syntax = NULLOID;
	if ((ps -> ps_defctx = oid_cpy (pb -> pb_asn)) == NULLOID)
	    goto congest;

	oid_free (pb -> pb_atn);
	pb -> pb_atn = cp_normal -> default__context -> transfer__syntax;
	cp_normal -> default__context -> transfer__syntax = NULLOID;
	if (!atn_is_ok (pb, pb -> pb_atn))
	    ps -> ps_defctxresult = PC_TRANSFER;

	pb -> pb_flags |= PB_DFLT;
    }
    pb -> pb_result = ps -> ps_defctxresult;

    if (cp_normal -> presentation__fu) {
	register struct pair *pp;

	if (!(pb -> pb_srequirements & SR_TYPEDATA)) {
	    (void) bit_off (cp_normal -> presentation__fu,
			bit_PS_Presentation__requirements_context__management);
	    (void) bit_off (cp_normal -> presentation__fu,
			    bit_PS_Presentation__requirements_restoration);
	}

	for (pp = preq_pairs; pp -> p_mask; pp++)
	    if (bit_test (cp_normal -> presentation__fu, pp -> p_bitno) == 1)
		pb -> pb_prequirements |= pp -> p_mask;
    }
    ps -> ps_prequirements = (pb -> pb_prequirements &= PR_MYREQUIRE);

    if (cp_normal -> session__fu) {
	register struct pair *pp;

	for (pp = sreq_pairs; pp -> p_mask; pp++)
	    if (bit_test (cp_normal -> session__fu, pp -> p_bitno) == 1)
		pb -> pb_urequirements |= pp -> p_mask;
    }
    else
	pb -> pb_urequirements = pb -> pb_srequirements;
    ps -> ps_srequirements = (pb -> pb_urequirements & pb -> pb_srequirements);

    ps -> ps_settings = ss -> ss_settings;
    ps -> ps_isn = ss -> ss_isn;

    ps -> ps_ssdusize = ss -> ss_ssdusize;

    ps -> ps_qos = ss -> ss_qos;	/* struct copy */

    pe_free (pe);
    free_PS_CP__type (cp);

    return OK;

congest: ;
    result = SC_CONGESTION;
    (void) psaplose (pi, result2 = PC_CONGEST, NULLCP, NULLCP);

    if (pe) {
	pe_free (pe);
	pe = NULLPE;
    }

    if (cpr = (struct type_PS_CPR__type *) calloc (1, sizeof *cpr)) {
	if (cp
	        && cp -> mode
	    	&& cp -> mode -> parm ==
	    			    int_PS_Mode__selector_x410__1984__mode) {
	    cpr -> offset = type_PS_CPR__type_x410__mode;
	    if (pe = pe_alloc (PE_CLASS_UNIV, PE_FORM_CONS, PE_CONS_SET)) {
		cpr -> un.x410__mode = pe;
		(void) set_add (pe, num2prim ((integer) (result2 != PC_CONGEST ? 0 : 3),
					      PE_CLASS_CONT, 0));
	    }
	}
	else {
	    register struct element_PS_2 *cpr_normal;

	    cpr -> offset = type_PS_CPR__type_normal__mode;
	    if (cpr_normal = (struct element_PS_2 *)
					    calloc (1, sizeof *cpr_normal)) {
		cpr -> un.normal__mode = cpr_normal;
		cpr_normal -> optionals |= opt_PS_element_PS_2_reason;
		cpr_normal -> reason = result2 - PC_PROV_BASE;
	    }
	}
    }

    if (encode_PS_CPR__type (&pe, 1, 0, NULLCP, cpr) != NOTOK) {
	PLOGP (psap2_log,PS_CPR__type, pe, "CPR-type", 0);

	if (pe)
	    (void) pe2ssdu (pe, &base, &len);
    }
    else
	base = NULL, len = 0;

    bzero ((char *) &ref, sizeof ref);
    (void) SConnResponse (ss -> ss_sd, &ref, NULLSA, result, 0, 0,
	    SERIAL_NONE, base, len, si);
    if (base)
	free (base);
    (void) psaplose (pi, result2, NULLCP, NULLCP);

out1: ;
    SSFREE (ss);
    if (pe)
	pe_free (pe);
    if (cp)
	free_PS_CP__type (cp);
    if (cpr)
	free_PS_CPR__type (cpr);
out2: ;
    freepblk (pb);

    return NOTOK;
}

/*    P-CONNECT.RESPONSE */

int	PConnResponse (sd, status, responding, ctxlist, defctxresult,
	prequirements, srequirements, isn, settings, ref, data, ndata, pi)
int	sd;
struct PSAPaddr *responding;
int	status,
	prequirements,
	srequirements,
	settings,
	ndata;
long	isn;
struct PSAPctxlist *ctxlist;
int	defctxresult;
struct SSAPref *ref;
PE     *data;
struct PSAPindication *pi;
{
    int	    i,
	    len,
	    result,
	    result2;
    char   *base;
    PE	    pe;
    register struct psapblk *pb;
    struct SSAPindication sis;
    register struct SSAPindication *si = &sis;
    register struct SSAPabort *sa = &si -> si_abort;
    struct type_PS_CPA__type *cpa;
    register struct element_PS_1 *cpa_normal;
    struct type_PS_CPR__type *cpr;
    register struct element_PS_2 *cpr_normal;

    if ((pb = findpblk (sd)) == NULL || (pb -> pb_flags & PB_CONN))
	return psaplose (pi, PC_PARAMETER, NULLCP,
		"invalid presentation descriptor");
#ifdef	notdef
    missingP (responding);
#endif
    switch (status) {
	case PC_ACCEPT:
	case PC_REJECTED:
	    break;

	default:
	    return psaplose (pi, PC_PARAMETER, NULLCP,
		    "bad value for status parameter");
    }
		    
    if (ctxlist) {
	register struct PSAPcontext *pp,
				    *qp;
	
	if (ctxlist -> pc_nctx != pb -> pb_ncontext)
	    return psaplose (pi, PC_PARAMETER, NULLCP,
		    "proposed/resulting presentation contexts mismatch");

	i = pb -> pb_ncontext - 1;
	for (pp = ctxlist -> pc_ctx, qp = pb -> pb_contexts; 
		i >= 0;
		i--, pp++, qp++) {
	    if (pp -> pc_id != qp -> pc_id)
		return psaplose (pi, PC_PARAMETER, NULLCP,
			"bad context id %d at offset %d (wanted %d)",
			pp -> pc_id, pp - ctxlist -> pc_ctx, qp -> pc_id);
	    switch (pp -> pc_result) {
		case PC_ACCEPT:
		case PC_REJECTED:
		    if (qp -> pc_result != PC_ACCEPT)
			return psaplose (pi, PC_PARAMETER, NULLCP,
				"invalid result %d for context id %d",
				pp -> pc_result, pp -> pc_id);
		    qp -> pc_result = pp -> pc_result;
		    break;

		default:
		    if (qp -> pc_result != pp -> pc_result)
			return psaplose (pi, PC_PARAMETER, NULLCP,
				"invalid result %d for context id %d",
				pp -> pc_result, pp -> pc_id);
		    break;
	    }
	}
    }

    switch (defctxresult) {
	case PC_ACCEPT:
	case PC_REJECTED:
	    if (pb -> pb_result != PC_ACCEPT)
		return psaplose (pi, PC_PARAMETER, NULLCP,
			"invalid result %d for default context name",
			defctxresult);
	    if ((pb -> pb_result = defctxresult) == PC_REJECTED
		    && status != PC_REJECTED)
		return psaplose (pi, PC_PARAMETER, NULLCP,
		       "default context rejected implies connection rejected");
	    break;

	default:
	    if (pb -> pb_result != defctxresult)
		return psaplose (pi, PC_PARAMETER, NULLCP,
			"invalid result %d for default context name",
			defctxresult);
	    break;
    }
    if (prequirements & ~pb -> pb_prequirements)
	return psaplose (pi, PC_PARAMETER, NULLCP,
		"presentation requirements not available");
    if (srequirements & ~pb -> pb_urequirements)
	return psaplose (pi, PC_PARAMETER, NULLCP,
		"session requirements not available");

/* let session provider catch errors in remainder of session parameters */

    toomuchP (data, ndata, NPDATA, "initial");
    missingP (pi);

    base = NULLCP;
    pe = NULLPE;
    cpa = NULL, cpr = NULL;

    switch (status) {
	case PC_ACCEPT:
	    if ((cpa = (struct type_PS_CPA__type *) calloc (1, sizeof *cpa))
		    == NULL) {
no_mem: ;
		(void) psaplose (pi, PC_CONGEST, NULLCP, "out of memory");
		goto out2;
	    }
	    if ((cpa -> mode = (struct type_PS_Mode__selector *)
				    malloc (sizeof *cpa -> mode)) == NULL)
		goto no_mem;
	    cpa -> mode -> parm = int_PS_Mode__selector_normal__mode;
	    if ((cpa_normal = (struct element_PS_1 *)
				    calloc (1, sizeof *cpa_normal)) == NULL)
		goto no_mem;
	    cpa -> normal__mode = cpa_normal;
	    break;

	case PC_REJECTED:
	    if ((cpr = (struct type_PS_CPR__type *) calloc (1, sizeof *cpr))
		    == NULL)
		goto no_mem;
	    cpr -> offset = type_PS_CPR__type_normal__mode;
	    if ((cpr_normal = (struct element_PS_2 *)
				    calloc (1, sizeof *cpr_normal)) == NULL)
		goto no_mem;
	    cpr -> un.normal__mode = cpr_normal;
	    break;
    }

    {
	register struct qbuf *qb = NULL;

	if (responding
	        && responding -> pa_selectlen > 0
	        && (qb = str2qb (responding -> pa_selector,
			       responding -> pa_selectlen, 1)) == NULL)
	    goto no_mem;

	if (status == PC_ACCEPT)
	    cpa_normal -> responding = qb;
	else
	    cpr_normal -> responding = qb;
    }

    if (pb -> pb_ncontext > 0) {
	register struct PSAPcontext *qp;
	register struct type_PS_Definition__result__list *cd,
							**cp;

	if (status == PC_ACCEPT)
	    cp = &cpa_normal -> context__list;
	else
	    cp = &cpr_normal -> context__list;

	i = pb -> pb_ncontext - 1;
	for (qp = pb -> pb_contexts; i >= 0; i--, qp++) {
	    switch (qp -> pc_result) {
		case PC_ACCEPT:
		    result = int_PS_result_acceptance;
		    break;

		case PC_REJECTED:
		    result = int_PS_result_user__rejection;
		    break;

		case PC_ABSTRACT:
		    result = int_PS_result_provider__rejection;
		    result2 = int_PS_provider__reason_abstract__syntax__not__supported;
		    break;

		case PC_TRANSFER:
		    result = int_PS_result_provider__rejection;
		    result2 = int_PS_provider__reason_proposed__transfer__syntaxes__not__supported;
		    break;

		case PC_CONGEST:
		    result = int_PS_result_provider__rejection;
		    result2 = int_PS_provider__reason_local__limit__on__DCS__exceeded;
		    break;

		case PC_NOTSPECIFIED:
		default:
		    result = int_PS_result_provider__rejection;
		    result2 = int_PS_provider__reason_reason__not__specified;
		    break;
	    }

	    if ((cd = (struct type_PS_Definition__result__list *)
		 	calloc (1, sizeof *cd)) == NULL)
		goto no_mem;
	    *cp = cd;
	    cp = &cd -> next;
	    if ((cd -> element_PS_12 = (struct element_PS_13 *)
			calloc (1, sizeof (struct element_PS_13))) == NULL)
		goto no_mem;
	    cd -> element_PS_12 -> result = result;
	    switch (qp -> pc_result) {
		case PC_ACCEPT:
		    if ((cd -> element_PS_12 -> transfer__syntax =
					    oid_cpy (qp -> pc_atn)) == NULLOID)
			goto no_mem;
		    break;

		case PC_REJECTED:
		    break;

		default:
		    cd -> element_PS_12 -> optionals |=
					opt_PS_element_PS_13_provider__reason;
		    cd -> element_PS_12 -> provider__reason = result2;
		    break;
	    }
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

    pb -> pb_srequirements &= srequirements;
    if (pb -> pb_prequirements & PR_MANAGEMENT)
	pb -> pb_srequirements |= SR_TYPEDATA;
    if (status == PC_ACCEPT) {
	if ((pb -> pb_prequirements &= prequirements) != PR_MYREQUIRE) {
	    register struct pair *pp;

	    if ((cpa_normal -> presentation__fu =
		 	prim2bit (pe_alloc (PE_CLASS_UNIV, PE_FORM_PRIM,
					    PE_PRIM_BITS))) == NULL)
		goto no_mem;

	    for (pp = preq_pairs; pp -> p_mask; pp++)
		if ((pb -> pb_prequirements & pp -> p_mask)
		    && bit_on (cpa_normal -> presentation__fu, pp -> p_bitno)
			    == NOTOK)
		    goto no_mem;

	    if (bit2prim (cpa_normal -> presentation__fu) == NULLPE)
		goto no_mem;
	}

	if ((pb -> pb_urequirements &= srequirements)
	        != pb -> pb_srequirements) {
	    register struct pair *pp;

	    if ((cpa_normal -> session__fu = prim2bit (pe_alloc (PE_CLASS_UNIV,
							     PE_FORM_PRIM,
							     PE_PRIM_BITS)))
	            == NULL)
		goto no_mem;

	    for (pp = sreq_pairs; pp -> p_mask; pp++)
		if ((pb -> pb_urequirements & pp -> p_mask)
		    && bit_on (cpa_normal -> session__fu, pp -> p_bitno)
		    	    == NOTOK)
		    goto no_mem;

	    if (bit2prim (cpa_normal -> session__fu) == NULLPE)
		goto no_mem;
	}

	status = SC_ACCEPT;
    }
    else {
	if (pb -> pb_flags & PB_DFLT) {
	    if ((cpr_normal -> default__context =
			(struct type_PS_Context__result *)
		        malloc (sizeof *cpr_normal -> default__context))
		     == NULL)
		goto no_mem;
	    switch (defctxresult) {
	        case PC_ACCEPT:
	        default:
	            result = int_PS_Result_acceptance;
		    break;
		
	        case PC_REJECTED:
		    result = int_PS_Result_user__rejection;
		    break;
		
	        case PC_NOTSPECIFIED:
		    result = int_PS_Result_provider__rejection;
		    break;
	    }
	    cpr_normal -> default__context -> parm = result;
	}

	status = SC_REJECTED;
    }

    if (data && ndata > 0) {
	struct type_PS_User__data *info;

	if ((info = info2ppdu (pb, pi, data, ndata, status == SC_ACCEPT
			       ? PPDU_CPA : PPDU_CPR)) == NULL)
	    goto out2;

	if (status == SC_ACCEPT)
	    cpa_normal -> user__data = info;
	else
	    cpr_normal -> user__data = info;
    }

    if (status == SC_ACCEPT) {
	result = encode_PS_CPA__type (&pe, 1, 0, NULLCP, cpa);

#ifdef	DEBUG
	if (result == OK && (psap2_log -> ll_events & LLOG_PDUS))
	    pvpdu (psap2_log, print_PS_CPA__type_P, pe, "CPA-type", 0);
#endif
    }
    else {
	result = encode_PS_CPR__type (&pe, 1, 0, NULLCP, cpr);

#ifdef	DEBUG
	if (result == OK && (psap2_log -> ll_events & LLOG_PDUS))
	    pvpdu (psap2_log, print_PS_CPR__type_P, pe, "CPR-type", 0);
#endif
    }

    if (result == NOTOK) {
	(void) psaplose (pi, PC_CONGEST, NULLCP, "error encoding PDU: %s",
			 PY_pepy);
	goto out2;
    }

    if (pe2ssdu (pe, &base, &len) == NOTOK)
	goto no_mem;

    if (status == SC_ACCEPT) {
	free_PS_CPA__type (cpa);
	cpa = NULL;
    }
    else {
	free_PS_CPR__type (cpr);
	cpr = NULL;
    }

    pe_free (pe);
    pe = NULL;
    
    if (SConnResponse (pb -> pb_fd, ref, responding ? &responding -> pa_addr
	    : NULLSA, status, pb -> pb_srequirements, settings, isn, base,
	    len, si) == NOTOK)
	if (SC_FATAL (sa -> sa_reason)) {
	    (void) ss2pslose (pb, pi, "SConnResponse", sa);
	    goto out2;
	}
	else {			/* assume SS-user parameter error */
	    result = ss2pslose (NULLPB, pi, "SConnResponse", sa);
	    goto out1;
	}

    if (status == SC_ACCEPT) {
	pb -> pb_flags |= PB_CONN;
#define dotoken(requires,shift,bit,type) \
{ \
	if (pb -> pb_srequirements & requires) \
	    switch (settings & (ST_MASK << shift)) { \
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
    }
    else {
	pb -> pb_fd = NOTOK;
	freepblk (pb);
    }
    result = OK;
    goto out1;

out2: ;
    result = NOTOK;
    if (cpr) {
	free_PS_CPR__type (cpr);
	cpr = NULL;
    }

    if (cpr = (struct type_PS_CPR__type *) calloc (1, sizeof *cpr)) {
	cpr -> offset = type_PS_CPR__type_normal__mode;
	if (cpr_normal = (struct element_PS_2 *)
	    	calloc (1, sizeof *cpr_normal)) {
	    cpr -> un.normal__mode = cpr_normal;
	    cpr_normal -> optionals |= opt_PS_element_PS_2_reason;
	    cpr_normal -> reason = int_PS_reason_temporary__congestion;
	}
    }

    if (encode_PS_CPR__type (&pe, 1, 0, NULLCP, cpr) != NOTOK) {
	PLOGP (psap2_log,PS_CPR__type, pe, "CPR-type", 0);

	if (pe)
	    (void) pe2ssdu (pe, &base, &len);
    }
    else
	base = NULL, len = 0;

    (void) SConnResponse (pb -> pb_fd, ref, NULLSA, result, 0, 0,
	    SERIAL_NONE, base, len, si);

    if (base)
	free (base);
    (void) pe2ssdu (pe, &base, &len);
    (void) SConnResponse (pb -> pb_fd, ref, NULLSA, SC_CONGESTION, 0, 0,
    		SERIAL_NONE, base, len, si);
			
    freepblk (pb);

out1: ;
    if (base)
	free (base);

    if (pe)
	pe_free (pe);
    if (cpa)
	free_PS_CPA__type (cpa);
    if (cpr)
	free_PS_CPR__type (cpr);

    return result;
}

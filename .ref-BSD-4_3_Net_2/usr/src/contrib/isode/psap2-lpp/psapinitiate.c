/* psapinitiate.c - PPM: initiator */

#ifndef	lint
static char *rcsid = "$Header: /f/osi/psap2-lpp/RCS/psapinitiate.c,v 7.5 91/02/22 09:38:06 mrose Interim $";
#endif

/* 
 * $Header: /f/osi/psap2-lpp/RCS/psapinitiate.c,v 7.5 91/02/22 09:38:06 mrose Interim $
 *
 * Contributed by The Wollongong Group, Inc.
 *
 *
 * $Log:	psapinitiate.c,v $
 * Revision 7.5  91/02/22  09:38:06  mrose
 * Interim 6.8
 * 
 * Revision 7.4  90/12/11  10:53:07  mrose
 * lock-and-load
 * 
 * Revision 7.3  90/07/09  14:45:05  mrose
 * sync
 * 
 * Revision 7.2  90/07/01  21:05:24  mrose
 * pepsy
 * 
 * Revision 7.1  89/12/01  10:51:43  mrose
 * touch-up
 * 
 * Revision 7.0  89/11/23  22:15:53  mrose
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
#define	LPP
#include "PS-types.h"
#include "ppkt.h"
#include "tailor.h"

/*    P-(ASYN-)CONNECT.REQUEST */

#ifndef	notdef
/* ARGSUSED */
#endif

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
    if (ctxlist == NULL || ctxlist -> pc_nctx != NPCTX_PS)
	return psaplose (pi, PC_PARAMETER, NULLCP,
			 "exactly %d proposed presentation contexts supported",
			 NPCTX_PS);
#ifdef	notdef
    if (defctxname)
	return psaplose (pi, PC_PARAMETER, NULLCP,
			 "default context name not allowed");
#endif
    if (prequirements != PR_KERNEL)
	return psaplose (pi, PC_PARAMETER, NULLCP,
			 "presentation requirements settings not supported");

    if (srequirements != SR_DUPLEX)
	return psaplose (pi, PC_PARAMETER, NULLCP,
			 "session requirements settings not supported");
    if (isn != SERIAL_NONE)
	return psaplose (pi, PC_PARAMETER, NULLCP,
			 "initial serial number not permitted");
    if (settings != 0)	/* not really an accurate test... */
	return psaplose (pi, PC_PARAMETER, NULLCP,
			 "initial token settings not permitted");
    missingP (ref);
    if (ref -> sr_ulen > SREF_USER_SIZE
	    || ref -> sr_ulen <= 2
	    || ref -> sr_clen > SREF_COMM_SIZE
	    || ref -> sr_clen <= 2
	    || ref -> sr_alen > SREF_ADDT_SIZE
	    || ref -> sr_alen == 1
	    || ref -> sr_vlen > 0)
	return psaplose (pi, PC_PARAMETER, NULLCP, "bad format for reference");
    if (data == NULL || ndata <= 0 || data[0] == NULLPE || ndata > NPDATA_PS)
	return psaplose (pi, PC_PARAMETER, NULLCP, "bad initial user data");
    if (data[0] -> pe_context != PCI_ACSE)
	return psaplose (pi, PC_PARAMETER, NULLCP,
			 "wrong context for initial user data");
    missingP (pc);
    missingP (pi);

    smask = sigioblock ();

    result = PConnRequestAux (calling, called, ctxlist, ref, data[0], qos,
			      pc, pi, async);

    (void) sigiomask (smask);

    return result;
}

/*  */

static int  PConnRequestAux (calling, called, ctxlist, ref, data, qos, pc, pi,
			     async)
struct PSAPaddr *calling,
		*called;
struct PSAPctxlist *ctxlist;
struct SSAPref *ref;
PE	data;
struct QOStype *qos;
struct PSAPconnect *pc;
struct PSAPindication *pi;
int	async;
{
    int	    result;
    OID     asn;
    register struct psapblk *pb;
    register struct type_PS_ConnectRequest__PDU *pdu;
    register struct type_PS_SessionConnectionIdentifier *pref;

    if ((pb = newpblk ()) == NULL)
	return psaplose (pi, PC_CONGEST, NULLCP, "out of memory");

    if ((pref = (struct type_PS_SessionConnectionIdentifier *)
	           malloc (sizeof *pref)) == NULL) {
	(void) psaplose (pi, PC_CONGEST, NULLCP, "out of memory");
	goto out1;
    }
    pb -> pb_reference = pref;
    pdu = NULL;
    if ((pref -> callingSSUserReference = str2qb (ref -> sr_udata + 2,
						  (int) ref -> sr_ulen - 2, 1))
		== NULL
	    || (pref -> commonReference = str2qb (ref -> sr_cdata + 2,
						  (int) ref -> sr_clen - 2, 1))
		== NULL) {
no_mem: ;
	(void) psaplose (pi, PC_CONGEST, NULLCP, "out of memory");
	goto out2;
    }
    if (ref -> sr_alen > 0) {
	if ((pref -> additionalReferenceInformation
					= str2qb (ref -> sr_adata + 2,
						  (int) ref -> sr_alen - 2, 1))
		== NULL)
	    goto no_mem;
    }
    else
	pref -> additionalReferenceInformation = NULL;

    if ((pb -> pb_ber = ode2oid (DFLT_ATN)) == NULLOID) {
	(void) psaplose (pi, PC_ABSTRACT, NULLCP, "%s: unknown", DFLT_ATN);
	goto out2;
    }
    if ((pb -> pb_ber = oid_cpy (pb -> pb_ber)) == NULLOID)
	goto no_mem;

    asn = NULLOID;
    {
	register int	i;
	register struct PSAPcontext *pp,
				    *qp;

	i = ctxlist -> pc_nctx - 1;
	for (pp = ctxlist -> pc_ctx, qp = pb -> pb_contexts;
	         i >= 0;
	         i--, pp++, qp++) {
	    switch (qp -> pc_id = pp -> pc_id) {
		case PCI_ROSE:
		    asn = pp -> pc_asn;	    /* and fall */
		case PCI_ACSE:
		    break;

		default:
		    (void) psaplose (pi, PC_PARAMETER, NULLCP,
				     "illegal value for PCI (%d)",
				     pp -> pc_id);
		goto out2;
	    }

	    if (pp -> pc_asn == NULLOID) {
		(void) psaplose (pi, PC_PARAMETER, NULLCP,
			    "no abstract syntax name given for context %d",
			    pp -> pc_id);
		goto out2;
	    }

	    if (pp -> pc_atn && !atn_is_ok (pb, pp -> pc_atn)) {
		(void) psaplose (pi, PC_TRANSFER, NULLCP,
			   "unknown transfer syntax given for context %d",
			   pp -> pc_id);
		goto out2;
	    }

	    qp -> pc_result = PC_ACCEPT;

	    pb -> pb_ncontext++;
	}
    }
    if (asn == NULLOID) {
	(void) psaplose (pi, PC_PARAMETER, NULLCP, "PCI for SASE not present");
	goto out2;
    }

    if ((pdu = (struct type_PS_ConnectRequest__PDU *) malloc (sizeof *pdu))
	    == NULL)
	goto no_mem;

    pdu -> version = int_PS_version_version__1;
    pdu -> reference = pref;
    if (calling && calling -> pa_selectlen > 0) {
	if ((pdu -> calling = str2qb (calling -> pa_selector,
				     calling -> pa_selectlen, 1)) == NULL)
	goto no_mem;
    }
    else
        pdu -> calling = NULL;

    if (called -> pa_selectlen > 0) {
	if ((pdu -> called = str2qb (called -> pa_selector,
				     called -> pa_selectlen, 1)) == NULL)
	    goto no_mem;
    }
    else
	pdu -> called = NULL;

    if ((pdu -> asn = oid_cpy (asn)) == NULLOID)
	goto no_mem;

    pdu -> user__data = data;

    pb -> pb_retry = NULLPE;
    result = encode_PS_ConnectRequest__PDU (&pb -> pb_retry, 1, 0, NULLCP,
					    pdu);

    pdu -> reference = NULL;
    pdu -> user__data = NULLPE;
    free_PS_ConnectRequest__PDU (pdu);
    pdu = NULL;

    if (result == NOTOK) {
	(void) psaplose (pi, PC_CONGEST, NULLCP, "error encoding PDU: %s",
			 PY_pepy);
	goto out1;
    }

    if ((result = PConnRequestAux2 (pb, calling ? &calling -> pa_addr.sa_addr
					    : NULLTA,
				    &called -> pa_addr.sa_addr, qos,
				    pi, async)) == NOTOK)
	goto out1;

    if (async && result == OK) {
	pc -> pc_sd = pb -> pb_fd;
	return result;
    }
    if ((result = PAsynRetryAux (pb, pc, pi)) == DONE && !async)
	result = OK;
    return result;

out2: ;
    if (pdu) {
	pdu -> reference = NULL;
	pdu -> user__data = NULLPE;
	free_PS_ConnectRequest__PDU (pdu);
    }

out1: ;
    freepblk (pb);

    return NOTOK;
}

/*  */

#define	QOS_RELIABLE_DFLT	HIGH_QUALITY


int	tcpopen (), udpopen ();

static struct nsapent {
    int     ns_reliability;
    int	    ns_tset;

    IFP	    ns_open;
}	nsaps[] = {
    HIGH_QUALITY, NA_TSET_TCP,  tcpopen,
    LOW_QUALITY,  NA_TSET_UDP,  udpopen,

    NULL
};


static int  PConnRequestAux2 (pb, calling, called, qos, pi, async)
struct psapblk *pb;
struct TSAPaddr *calling,
		*called;
struct QOStype *qos;
struct PSAPindication *pi;
int	async;
{
    int	    reliability,
	    result;
    register int n = called -> ta_naddr - 1;
    register struct NSAPaddr *na = called -> ta_addrs;

    reliability = qos ? qos -> qos_reliability : QOS_RELIABLE_DFLT;

    for (; n >= 0; na++, n--) {
	register int	l;
	register struct NSAPaddr *la;
	register struct nsapent *ns;

	if (na -> na_stack != NA_TCP)
	    continue;

	if (na -> na_tset == 0)
	    na -> na_tset = NA_TSET_TCP;

	for (ns = nsaps; ns -> ns_open; ns++)
	    if (ns -> ns_reliability == reliability
		    && (ns -> ns_tset & na -> na_tset))
		break;
	if (!ns -> ns_open)
	    continue;

	if (calling) {
	    for (l = calling -> ta_naddr - 1, la = calling -> ta_addrs;
		    l >= 0;
		    la++, l--) {
		if (la -> na_stack != NA_TCP)
		    continue;
		if (ns -> ns_tset & la -> na_tset)
		    break;
	    }
	    if (l < 0)
		la = NULLNA;
	}
	else
	    la = NULLNA;

        if ((result = (*ns -> ns_open) (pb, la, na, pi, async)) != NOTOK)
	    break;
    }

    {
	register struct TSAPaddr *ta = &pb -> pb_responding.pa_addr.sa_addr;

	ta -> ta_addrs[0] = *na;	/* struct copy */
	ta -> ta_naddr = 1;
    }

    return (pb -> pb_fd != NOTOK ? result : NOTOK);
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

    switch (result = (*pb -> pb_retryfnx) (pb, PC_REFUSED, pi)) {
	case NOTOK: 
	    pb -> pb_fd = NOTOK;
	    freepblk (pb);
	    break;

	case OK: 
	    break;

	case DONE: 
	    result = PAsynRetryAux (pb, pc, pi);
	    break;
    }

    (void) sigiomask (smask);

    return result;
}

/*  */

static int  PAsynRetryAux (pb, pc, pi)
register struct psapblk *pb;
struct PSAPconnect *pc;
struct PSAPindication *pi;
{
    int	    result;
    PE	    pe;
    struct type_PS_PDUs *pdu;

    pdu = NULL;
    result = decode_PS_PDUs (pb -> pb_response, 1, NULLIP, NULLVP, &pdu);

#ifdef	DEBUG
    if (result == OK && (psap2_log -> ll_events & LLOG_PDUS))
	pvpdu (psap2_log, print_PS_PDUs_P, pb -> pb_response, "PDU", 1);
#endif

    if (pb -> pb_retry) {
	pe_free (pb -> pb_retry);
	pb -> pb_retry = NULLPE;
    }

    pe_free (pb -> pb_response);
    pb -> pb_response = NULL;
    
    if (result == NOTOK) {
	(void) ppktlose (pb, pi, PC_UNRECOGNIZED, NULLRF, NULLCP,
			 "error decoding PDU: %s", PY_pepy);
	goto out;
    }

    bzero ((char *) pc, sizeof *pc);

    switch (pdu -> offset) {
	case type_PS_PDUs_connectResponse:
	{
	    register struct type_PS_ConnectResponse__PDU *cr =
				pdu -> un.connectResponse;

	    if (pb -> pb_reliability == LOW_QUALITY
		    && refcmp (pb -> pb_reference, cr -> reference)) {
		result = ppktlose (pb, pi, PC_SESSION, cr -> reference,
				   NULLCP, "reference mismatch");
		goto out;
	    }

	    if (cr -> reason == NULL) {
		pb -> pb_flags |= PB_CONN;

		pc -> pc_sd = pb -> pb_fd;
		pc -> pc_result = PC_ACCEPT;
		pc -> pc_qos.qos_reliability = pb -> pb_reliability;
		pc -> pc_qos.qos_sversion = 2;
	    }
	    else {
		pc -> pc_sd = NOTOK;
		pc -> pc_result = cr -> reason -> parm;
	    }

	    pdu2sel (pb -> pb_responding.pa_selector,
		     &pb -> pb_responding.pa_selectlen,
		     sizeof pb -> pb_responding.pa_selector,
		     cr -> responding);
	    pc -> pc_responding = pb -> pb_responding;	/* struct copy */

	    pc -> pc_defctxresult = pb -> pb_result = PC_ACCEPT;
	    {
		register int	i;
		register struct PSAPcontext *pp,
					    *qp;

		i = pb -> pb_ncontext;
		for (pp = pb -> pb_contexts, qp = pc -> pc_ctxlist.pc_ctx;
			i >= 0;
		        i--, pp++, qp++) {
		    qp -> pc_id = pp -> pc_id;
		    qp -> pc_asn = qp -> pc_atn = NULLOID;
		    qp -> pc_result = PC_ACCEPT;
		}
		pc -> pc_ctxlist.pc_nctx = pb -> pb_ncontext;
	    }

	    pc -> pc_prequirements = PR_KERNEL;
	    pc -> pc_srequirements = SR_DUPLEX;

	    pc -> pc_isn = SERIAL_NONE;

	    pc -> pc_connect = *pdu2ref (pb -> pb_reference); /* struct copy */

	    pe = cr -> user__data, cr -> user__data = NULLPE;
	    if (pc -> pc_info[0] = pe) {
		pe -> pe_context = PCI_ACSE;
		pc -> pc_ninfo = 1;
	    }

	    free_PS_PDUs (pdu);

	    return DONE;
	}

	case type_PS_PDUs_abort:
	{
	    register struct PSAPabort *pa = &pi -> pi_abort;
	    register struct type_PS_Abort__PDU *ab = pdu -> un.abort;

	    if (pb -> pb_reliability == LOW_QUALITY
		    && refcmp (pb -> pb_reference, ab -> reference)) {
		result = psaplose (pi, PC_SESSION, NULLCP,
				   "reference mismatch");
		goto out;
	    }

	    if (ab -> reason) {
		switch (ab -> reason -> parm) {
		    case int_PS_Abort__reason_reason__not__specified:
		    default:
		        result = PC_NOTSPECIFIED;
		        break;

		    case int_PS_Abort__reason_unrecognized__ppdu:
		    case int_PS_Abort__reason_unexpected__ppdu:
		    case int_PS_Abort__reason_unrecognized__ppdu__parameter:
			result = PC_UNRECOGNIZED
				+ (ab -> reason -> parm
			            - int_PS_Abort__reason_unrecognized__ppdu);
			break;

		    case int_PS_Abort__reason_invalid__ppdu__parameter:
			result = PC_INVALID;
			break;

		    case int_PS_Abort__reason_reference__mismatch:
			result = PC_SESSION;
			break;
		}
		result = psaplose (pi, result, NULLCP, NULLCP);
		goto out;
	    }
	    pe = ab -> user__data, ab -> user__data = NULLPE;

	    pi -> pi_type = PI_ABORT;
	    bzero ((char *) pa, sizeof *pa);

	    pa -> pa_peer = 1;
	    pa -> pa_reason = PC_ABORTED;
	    if (pa -> pa_info[0] = pe) {
		pe -> pe_context = PCI_ACSE;
		pa -> pa_ninfo = 1;
	    }

	    pc -> pc_sd = NOTOK;
	    pc -> pc_result = PC_ABORTED;

	    result = DONE;
	}
	break;

	default:
	/* this works 'cause the "reference" is always the FIRST element */
	    result = ppktlose (pb, pi, PC_SESSION,
			       pdu -> un.connectResponse -> reference, NULLCP,
			       "unexpected PDU %d", pdu -> offset);
	    break;
    }

out: ;
    if (pdu)
	free_PS_PDUs (pdu);
    freepblk (pb);

    return result;
}

/*    P-ASYN-NEXT.REQUEST (pseudo) */

/* ARGSUSED */

int	PAsynNextRequest (sd, pc, pi)
int	sd;
struct PSAPconnect *pc;
struct PSAPindication *pi;
{
    return psaplose (pi, PC_OPERATION, NULLCP,
		     "operation not supported with lightweight presentation");
}

/* psaprovider.c - PPM: implement the pseudo-presentation protocol */

#ifndef	lint
static char *rcsid = "$Header: /f/osi/psap2-lpp/RCS/psaprovider.c,v 7.4 91/02/22 09:38:15 mrose Interim $";
#endif

/* 
 * $Header: /f/osi/psap2-lpp/RCS/psaprovider.c,v 7.4 91/02/22 09:38:15 mrose Interim $
 *
 * Contributed by The Wollongong Group, Inc.
 *
 *
 * $Log:	psaprovider.c,v $
 * Revision 7.4  91/02/22  09:38:15  mrose
 * Interim 6.8
 * 
 * Revision 7.3  91/01/07  12:40:43  mrose
 * update
 * 
 * Revision 7.2  90/08/08  14:02:42  mrose
 * stuff
 * 
 * Revision 7.1  90/07/01  21:05:36  mrose
 * pepsy
 * 
 * Revision 7.0  89/11/23  22:16:00  mrose
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

/*    DATA */

static int  once_only = 0;
static struct psapblk psapque;
static struct psapblk *PHead = &psapque;


int	_iosignals_set = 0;

/*    P-DATA.REQUEST */

int	PDataRequest (sd, data, ndata, pi)
int	sd;
PE     *data;
int	ndata;
struct PSAPindication *pi;
{
    SBV	    smask;
    int	    result;
    register struct psapblk *pb;

    if (data == NULL || ndata <= 0 || data[0] == NULLPE || ndata > NPDATA_PS)
	return psaplose (pi, PC_PARAMETER, NULLCP, "bad user data");
    if (data[0] -> pe_context != PCI_ROSE)
	return psaplose (pi, PC_PARAMETER, NULLCP,
			 "wrong context for user data");
    missingP (pi);

    smask = sigioblock ();

    psapPsig (pb, sd);

    if ((result = PDataRequestAux (pb, data[0], pi)) == NOTOK)
	freepblk (pb);

    (void) sigiomask (smask);

    return result;
}

/*  */

static int  PDataRequestAux (pb, data, pi)
register struct psapblk *pb;
PE	data;
struct PSAPindication *pi;
{
    int	    result;
#ifdef	DEBUG
    char   *cp;
#endif
    PE	    pe;
    PS	    ps;

    pe = NULLPE;

    if (pb -> pb_reliability == LOW_QUALITY) {
	struct type_PS_CL__UserData__PDU *pdu;

	if ((pdu = (struct type_PS_CL__UserData__PDU *) malloc (sizeof *pdu))
	        == NULL)
	    return psaplose (pi, PC_CONGEST, NULLCP, "out of memory");

	pdu -> reference = pb -> pb_reference;
	pdu -> user__data = data;

	result = encode_PS_CL__UserData__PDU (&pe, 1, 0, NULLCP, pdu);
#ifdef	DEBUG
	cp = "CL-UserData-PDU";
#endif

	pdu -> reference = NULL;
	pdu -> user__data = NULLPE;
	free_PS_CL__UserData__PDU (pdu);
    }
    else {
	result = encode_PS_UserData__PDU (&pe, 1, 0, NULLCP, data);
#ifdef	DEBUG
	cp = "UserData-PDU";
#endif
    }

    if (result != NOTOK) {
	PLOGP (psap2_log,PS_PDUs, pe, cp, 0);

	if ((result = pe2ps (ps = pb -> pb_stream, pe)) == NOTOK)
	    (void) pslose (pi, ps -> ps_errno);
	else
	    result = OK;
    }
    else
	(void) psaplose (pi, PC_CONGEST, NULLCP, "error encoding PDU: %s",
			 PY_pepy);

    if (pe)
	pe_free (pe);

    return result;
}

/*    P-READ.REQUEST (pseudo; synchronous read) */

int	PReadRequest (sd, px, secs, pi)
int	sd;
struct PSAPdata *px;
int	secs;
struct PSAPindication *pi;
{
    SBV	    smask;
    int	    nfds,
	    result;
    fd_set  mask;
    register struct psapblk *pb;
    struct PSAPabort *pa = &pi -> pi_abort;

    missingP (px);
    missingP (pi);

    smask = sigioblock ();

    psapPsig (pb, sd);

    FD_ZERO (&mask);
    FD_SET (pb -> pb_fd, &mask);
    nfds = pb -> pb_fd + 1;

    for (;;) {
	fd_set	 ifds,
		 efds;
	register PS 	ps = pb -> pb_stream;

	ifds = mask;	/* struct copy */
	efds = mask;	/* struct copy */

	if (ps_prime (ps, 1) == OK
	        && (*pb -> pb_selectfnx) (nfds, &ifds, NULLFD, &efds, secs)
			<= OK) {
	    result = psaplose (pi, PC_TIMER, NULLCP, NULLCP);
	    break;
	}

	if (FD_ISSET (pb -> pb_fd, &ifds) || FD_ISSET (pb -> pb_fd, & efds))
	    if ((result = PReadRequestAux (pb, px, pi)) != NOTOK
		    || secs != NOTOK
		    || pa -> pa_reason != PC_TIMER)
		break;
    }

    if (result == NOTOK && pa -> pa_reason != PC_TIMER)
	freepblk (pb);
	
    (void) sigiomask (smask);

    return result;
}

/*  */

static int  PReadRequestAux (pb, px, pi)
register struct psapblk *pb;
struct PSAPdata *px;
struct PSAPindication *pi;
{
    int	    result;
    PE	    pe;
    PS      ps;
    struct type_PS_PDUs *pdu;
    struct type_PS_SessionConnectionIdentifier *pref;

    if ((pe = ps2pe (ps = pb -> pb_stream)) == NULLPE)
	return pslose (pi, ps -> ps_errno);

    pdu = NULL;
    result = decode_PS_PDUs (pe, 1, NULLIP, NULLVP, &pdu);

#ifdef	DEBUG
    if (result == OK && (psap2_log -> ll_events & LLOG_PDUS))
	pvpdu (psap2_log, print_PS_PDUs_P, pe, "PDU", 1);
#endif

    pe_free (pe);

    if (result == NOTOK) {
	if (pb -> pb_reliability == LOW_QUALITY)
	    goto bad_ref2;

	(void) ppktlose (pb, pi, PC_UNRECOGNIZED, NULLRF, NULLCP,
			 "error decoding PDU: %s", PY_pepy);
	goto out;
    }

    switch (pdu -> offset) {
	case type_PS_PDUs_releaseRequest:
	{
	    register struct PSAPfinish *pf = &pi -> pi_finish;
	    register struct type_PS_ReleaseRequest__PDU *rr =
				    pdu -> un.releaseRequest;

	    if (pb -> pb_reliability == LOW_QUALITY
		    && refcmp (pb -> pb_reference, (pref = rr -> reference))) {
bad_ref1: ;
		(void) ppktlose (pb, pi, PC_SESSION, pref, NULLCP,
				 "reference mismatch");
bad_ref2: ;
		if (pdu)
		    free_PS_PDUs (pdu);
		return psaplose (pi, PC_TIMER, NULLCP, NULLCP);
	    }
	    
	    pe = rr -> user__data, rr -> user__data = NULLPE;

	    pi -> pi_type = PI_FINISH;
	    bzero ((char *) pf, sizeof *pf);

	    (pf -> pf_info[0] = pe) -> pe_context = PCI_ACSE;
	    pf -> pf_ninfo = 1;

	    pb -> pb_flags |= PB_FINN;
	    result = DONE;
	}
	break;

	case type_PS_PDUs_abort:
	{
	    register struct PSAPabort *pa = &pi -> pi_abort;
	    register struct type_PS_Abort__PDU *ab = pdu -> un.abort;

	    if (pb -> pb_reliability == LOW_QUALITY
		    && refcmp (pb -> pb_reference, (pref = ab -> reference)))
		goto bad_ref2;

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
		break;
	    }
	    pe = ab -> user__data, ab -> user__data = NULLPE;

	    pi -> pi_type = PI_ABORT;
	    bzero ((char *) pa, sizeof *pa);

	    pa -> pa_peer = 1;
	    pa -> pa_reason = PC_ABORTED;
	    (pa -> pa_info[0] = pe) -> pe_context = PCI_ACSE;
	    pa -> pa_ninfo = 1;

	    result = NOTOK;
	}
	break;

	case type_PS_PDUs_userData:
	{
	    if (pb -> pb_reliability == LOW_QUALITY)
		goto bad_ref2;

	    pe = pdu -> un.userData, pdu -> un.userData = NULLPE;

	    bzero ((char *) px, sizeof *px);

	    px -> px_type = SX_NORMAL;
	    (px -> px_info[0] = pe) -> pe_context = PCI_ROSE;
	    px -> px_ninfo = 1;

	    result = OK;
	}
	break;

	case type_PS_PDUs_cL__userData:
	{
	    register struct type_PS_CL__UserData__PDU *cl =
				    pdu -> un.cL__userData;

	    if (pb -> pb_reliability == LOW_QUALITY
		    && refcmp (pb -> pb_reference, (pref = cl -> reference)))
		goto bad_ref1;

	    pe = cl -> user__data, cl -> user__data = NULLPE;

	    bzero ((char *) px, sizeof *px);

	    px -> px_type = SX_NORMAL;
	    (px -> px_info[0] = pe) -> pe_context = PCI_ROSE;
	    px -> px_ninfo = 1;

	    result = OK;
	}
	break;

	case type_PS_PDUs_connectRequest:
	/* this works 'cause the "reference" is always the FIRST element */
	    result = ppktlose (pb, pi, PC_SESSION,
			       pdu -> un.connectRequest -> reference, NULLCP,
			       "unexpected PDU %d", pdu -> offset);
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

    return result;
}

/*    define vectors for INDICATION events */

/* ARGSUSED */

int	PSetIndications (sd, data, tokens, sync, activity, report, finish,
			 abort, pi)
int	sd;
IFP	data,
	tokens,
	sync,
	activity,
	report,
	finish,
	abort;
struct PSAPindication *pi;
{
    missingP (pi);

    return psaplose (pi, PC_OPERATION, NULLCP, NULLCP);
}

/*    INTERNAL */

struct psapblk  *newpblk () {
    register struct psapblk *pb;

    pb = (struct psapblk   *) calloc (1, sizeof *pb);
    if (pb == NULL)
	return NULL;

    pb -> pb_fd = NOTOK;

    if (once_only == 0) {
	PHead -> pb_forw = PHead -> pb_back = PHead;
	once_only++;
    }

    insque (pb, PHead -> pb_back);

    return pb;
}


int	freepblk (pb)
register struct psapblk *pb;
{
#ifdef	notdef
    register int    i;
    register struct PSAPcontext *qp;
#endif

    if (pb == NULL)
	return;

    if (pb -> pb_fd != NOTOK && pb -> pb_closefnx)
	(void) (*pb -> pb_closefnx) (pb -> pb_fd);

    if (pb -> pb_retry)
	pe_free (pb -> pb_retry);
    if (pb -> pb_response)
	pe_free (pb -> pb_response);
    if (pb -> pb_reference)
	free_PS_SessionConnectionIdentifier (pb -> pb_reference);
    if (pb -> pb_stream)
	ps_free (pb -> pb_stream);

#ifdef	notdef	/* don't need this stuff */
    for (qp = pb -> pb_contexts, i = pb -> pb_ncontexts - 1;
	    i >= 0;
	    qp++, i--) {
	if (qp -> pc_asn)
	    oid_free (qp -> pc_asn);
	if (qp -> pc_atn)
	    oid_free (qp -> pc_atn);
    }
    if (pb -> pb_asn)
	oid_free (pb -> pb_asn);
    if (pb -> pb_atn)
	oid_free (pb -> pb_atn);
#endif

    if (pb -> pb_ber)
	oid_free (pb -> pb_ber);

    remque (pb);

    free ((char *) pb);
}

/*  */

struct psapblk   *findpblk (sd)
register int sd;
{
    register struct psapblk *pb;

    if (once_only == 0)
	return NULL;

    for (pb = PHead -> pb_forw; pb != PHead; pb = pb -> pb_forw)
	if (pb -> pb_fd == sd)
	    return pb;

    return NULL;
}

/*  */

int	refcmp (ref1, ref2)
register struct type_PS_SessionConnectionIdentifier *ref1,
                                                    *ref2;
{
    if (ref1 == NULLRF)
	return (ref2 != NULLRF);
    else
	if (ref2 == NULLRF)
	    return 1;

    if (qb_cmp (ref1 -> callingSSUserReference, ref2 -> callingSSUserReference)
	    || qb_cmp (ref1 -> commonReference, ref2 -> commonReference)
	    || qb_cmp (ref1 -> additionalReferenceInformation,
		       ref2 -> additionalReferenceInformation)) {
	SLOG (psap2_log, LLOG_EXCEPTIONS, NULLCP, ("reference mismatch"));

	return 1;
    }

    return 0;
}


static int  qb_cmp (qb1, qb2)
register struct qbuf *qb1,
		     *qb2;
{
    register int    i,
		    len1,
		    len2;
    register char  *cp1,
		   *cp2;
    register struct qbuf *qp1,
			 *qp2;

    if (qb1 == NULL)
	return (qb2 != NULL);
    else
	if (qb2 == NULL)
	    return 1;
	    
    for (qp1 = qb1 -> qb_forw; qp1 != qb1; qp1 = qp1 -> qb_forw)
	if ((len1 = qp1 -> qb_len) > 0)
	    break;
    cp1 = qp1 -> qb_data;

    for (qp2 = qb2 -> qb_forw; qp2 != qb2; qp2 = qp2 -> qb_forw)
	if ((len2 = qp2 -> qb_len) > 0)
	    break;
    cp2 = qp2 -> qb_data;

    for (;;) {
	if (qp1 == qb1)
	    return (qb2 != qb2);
	else
	    if (qp2 == qb2)
		return 1;

	if ((i = len1) > len2)
	    i = len2;
	if (bcmp (cp1, cp2, i))
	    return 1;

	if ((len1 -= i) <= 0) {
	    for (qp1 = qp1 -> qb_forw; qp1 != qb1; qp1 = qp1 -> qb_forw)
		if ((len1 = qp1 -> qb_len) > 0)
		    break;
	    cp1 = qp1 -> qb_data;
	}
	else
	    cp1 += i;
	
	if ((len2 -= i) <= 0) {
	    for (qp2 = qp2 -> qb_forw; qp2 != qb2; qp2 = qp2 -> qb_forw)
		if ((len2 = qp2 -> qb_len) > 0)
		    break;
	    cp2 = qp2 -> qb_data;
	}
	else
	    cp2 += i;
    }
}

/*  */

struct SSAPref *pdu2ref (ref)
register struct type_PS_SessionConnectionIdentifier *ref;
{
    int	    i;
    static struct SSAPref sfs;
    register struct SSAPref *sf = &sfs;

    pdu2sel (sf -> sr_udata, &i, sizeof sf -> sr_udata,
	     ref -> callingSSUserReference);
    sf -> sr_ulen = i;

    pdu2sel (sf -> sr_cdata, &i, sizeof sf -> sr_cdata,
	     ref -> commonReference);
    sf -> sr_clen = i;

    pdu2sel (sf -> sr_adata, &i, sizeof sf -> sr_adata,
	     ref -> additionalReferenceInformation);
    sf -> sr_alen = i;

    sf -> sr_vlen = 0;

    return sf;
}

/*  */

int	pdu2sel (sel, len, i, pb)
char   *sel;
int    *len;
register int i;
register struct qbuf *pb;
{
    register char  *cp;
    register struct qbuf *qb;

    if (pb == NULL) {
	*len = 0;
	return;
    }

    cp = sel;
    for (qb = pb -> qb_forw; qb != pb && i > 0; qb = qb -> qb_forw) {
	if (qb -> qb_len > i)
	    qb -> qb_len = i;
	bcopy (qb -> qb_data, cp, qb -> qb_len);
	cp += qb -> qb_len, i -= qb -> qb_len;
    }

    *len = cp - sel;
}

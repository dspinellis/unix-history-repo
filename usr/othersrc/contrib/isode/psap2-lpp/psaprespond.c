/* psaprespond.c - PPM: responder */

#ifndef	lint
static char *rcsid = "$Header: /f/osi/psap2-lpp/RCS/psaprespond.c,v 7.2 91/02/22 09:38:13 mrose Interim $";
#endif

/* 
 * $Header: /f/osi/psap2-lpp/RCS/psaprespond.c,v 7.2 91/02/22 09:38:13 mrose Interim $
 *
 * Contributed by The Wollongong Group, Inc.
 *
 *
 * $Log:	psaprespond.c,v $
 * Revision 7.2  91/02/22  09:38:13  mrose
 * Interim 6.8
 * 
 * Revision 7.1  90/07/01  21:05:32  mrose
 * pepsy
 * 
 * Revision 7.0  89/11/23  22:15:58  mrose
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
#define	LPP
#include "PS-types.h"
#include "ppkt.h"
#include "tailor.h"


#define	AC_ASN		"acse pci version 1"

/*    P-CONNECT.INDICATION */

int	PInit (vecp, vec, ps, pi)
int	vecp;
char  **vec;
struct PSAPstart *ps;
struct PSAPindication *pi;
{
    register struct psapblk *pb;

    isodetailor (NULLCP, 0);

    if (vecp < 2)
	return psaplose (pi, PC_PARAMETER, NULLCP,
			 "bad initialization vector");
    missingP (vec);
    missingP (ps);
    missingP (pi);

    if ((pb = newpblk ()) == NULL)
	return psaplose (pi, PC_CONGEST, NULLCP, "out of memory");

    vec++, vecp--;
    switch (*vec[0]) {
	case PT_TCP:
	    if (tcprestore (pb, vec[0] + 1, pi) == NOTOK)
		goto out;
	    break;	

	case PT_UDP:
	    if (udprestore (pb, vec[0] + 1, pi) == NOTOK)
		goto out;
	    break;	

	default:
	    (void) psaplose (pi, PC_PARAMETER, NULLCP,
			     "unknown transport type: 0x%x (%c)",
			     *vec[0], *vec[0]);
	    goto out;
    }
    *vec = NULL;
    if (PInitAux (pb, ++vec, --vecp, ps, pi) == NOTOK)
	goto out;

    return OK;

out: ;
    freepblk (pb);

    return NOTOK;
}

/*  */

/* ARGSUSED */

static int  PInitAux (pb, vec, vecp, ps, pi)
register struct psapblk *pb;
char  **vec;
int	vecp;
struct PSAPstart *ps;
struct PSAPindication *pi;
{
    int	    result;
    PE	    pe;
    OID	    oid;
    register struct PSAPcontext *pp;
    struct type_PS_PDUs *pdu;
    register struct type_PS_ConnectRequest__PDU *cr;
    register struct TSAPaddr *ta;

    if ((pe = ps2pe (pb -> pb_stream)) == NULLPE)
	return pslose (pi, pb -> pb_stream -> ps_errno);

    pdu = NULL;
    oid = NULLOID;
    result = decode_PS_PDUs (pe, 1, NULLIP, NULLVP, &pdu);

#ifdef	DEBUG
    if (result == OK && (psap2_log -> ll_events & LLOG_PDUS))
	pvpdu (psap2_log, print_PS_PDUs_P, pe, "PDU", 1);
#endif

    pe_free (pe);

    if (result == NOTOK) {
	(void) ppktlose (pb, pi, PC_UNRECOGNIZED, NULLRF, NULLCP,
			 "error decoding PDU: %s", PY_pepy);
	goto out;
    }

    if (pdu -> offset != type_PS_PDUs_connectRequest) {
	/* this works 'cause the "reference" is always the FIRST element */
	if (pdu -> offset != type_PS_PDUs_abort)
	    result = ppktlose (pb, pi, PC_SESSION,
			       pdu -> un.connectResponse -> reference, NULLCP,
			       "unexpected PDU %d", pdu -> offset);
	else
	    result = psaplose (pi, PC_SESSION, NULLCP, "unexpected PDU %d",
			       pdu -> offset);
	goto out;
    }

    cr = pdu -> un.connectRequest;

    if ((oid = ode2oid (AC_ASN)) == NULL) {
	result = ppktlose (pb, pi, PC_PARAMETER, cr -> reference, NULLCP,
			   "%s: unknown", AC_ASN);
	goto out;
    }
    if ((oid = oid_cpy (oid)) == NULL) {
	result = ppktlose (pb, pi, PC_CONGEST, cr -> reference, NULLCP,
			   NULLCP);
	goto out;
    }

    pb -> pb_reference = cr -> reference, cr -> reference = NULL;
    pe = cr -> user__data, cr -> user__data = NULLPE;

/* should check version number here... */

    bzero ((char *) ps, sizeof *ps);

    ps -> ps_sd = pb -> pb_fd;

    ta = &ps -> ps_calling.pa_addr.sa_addr;
    ta -> ta_naddr = 1;
    ta -> ta_addrs[0] = pb -> pb_initiating;	/* struct copy */
    pdu2sel (ps -> ps_calling.pa_selector,
	     &ps -> ps_calling.pa_selectlen,
	     sizeof ps -> ps_calling.pa_selector,
	     cr -> calling);

    ta = &pb -> pb_responding.pa_addr.sa_addr;
    ta -> ta_naddr = 1;
    ta -> ta_addrs[0] = pb -> pb_initiating;	/* struct copy */
    pdu2sel (pb -> pb_responding.pa_selector,
	     &pb -> pb_responding.pa_selectlen,
	     sizeof pb -> pb_responding.pa_selector,
	     cr -> called);
    ps -> ps_called = pb -> pb_responding;	/* struct copy */

    ps -> ps_ctxlist.pc_nctx = 2;
    pp = ps -> ps_ctxlist.pc_ctx;

    pp -> pc_id = PCI_ROSE;
    pp -> pc_asn = cr -> asn;
    pp -> pc_result = PC_ACCEPT;
    cr -> asn = NULLOID, pp++;

    pp -> pc_id = PCI_ACSE;
    pp -> pc_asn = oid, oid = NULLOID;
    pp -> pc_result = PC_ACCEPT;

    ps -> ps_defctxresult = PC_ACCEPT;

    ps -> ps_prequirements = PR_KERNEL;
    ps -> ps_srequirements = SR_DUPLEX;

    ps -> ps_isn = SERIAL_NONE;

    ps -> ps_connect = *pdu2ref (pb -> pb_reference);	/* struct copy */

    ps -> ps_qos.qos_reliability = pb -> pb_reliability;
    ps -> ps_qos.qos_sversion = 2;

    (ps -> ps_info[0] = pe) -> pe_context = PCI_ACSE;
    ps -> ps_ninfo = 1;

    result = OK;

out: ;
    if (pdu)
	free_PS_PDUs (pdu);
    if (oid)
	oid_free (oid);

    return result;
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
    int	    result;
    PE	    pe;
    PS	    ps;
    register struct psapblk *pb;
    register struct type_PS_ConnectResponse__PDU *pdu;

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
    if (ctxlist != NULL) {
	register int	i;
	register struct PSAPcontext *pp;

	i = ctxlist -> pc_nctx - 1;
	for (pp = ctxlist -> pc_ctx; i >= 0; i--, pp++) {
	    switch (pp -> pc_id) {
		case PCI_ACSE:
		case PCI_ROSE:
		    break;

		default:
		    return psaplose (pi, PC_PARAMETER, NULLCP,
				     "illegal value for PCI (%d)",
				     pp -> pc_id);
	    }
	    if (pp -> pc_result != PC_ACCEPT)
		return psaplose (pi, PC_PARAMETER, NULLCP,
				 "must accept PCI %d", pp -> pc_id);
	}
    }
    if (defctxresult != PC_ACCEPT)
	return psaplose (pi, PC_PARAMETER, NULLCP,
			 "must accept non-existant default context");
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
	    || ref -> sr_clen > SREF_COMM_SIZE
	    || ref -> sr_alen > SREF_ADDT_SIZE
	    || ref -> sr_vlen > 0)
	return psaplose (pi, PC_PARAMETER, NULLCP, "bad format for reference");
    if (data == NULL || ndata <= 0 || data[0] == NULLPE || ndata > NPDATA_PS)
	return psaplose (pi, PC_PARAMETER, NULLCP, "bad initial user data");
    if (data[0] -> pe_context != PCI_ACSE)
	return psaplose (pi, PC_PARAMETER, NULLCP,
			 "wrong context for initial user data");
    missingP (pi);

    if ((pdu = (struct type_PS_ConnectResponse__PDU *) malloc (sizeof *pdu))
	    == NULL) {
no_mem: ;
	(void) psaplose (pi, PC_CONGEST, NULLCP, "out of memory");
	goto out2;
    }

    pdu -> reference = pb -> pb_reliability == LOW_QUALITY ? pb -> pb_reference
							   : NULLRF;

    if (responding && responding -> pa_selectlen > 0) {
	if ((pdu -> responding = str2qb (responding -> pa_selector,
				     responding -> pa_selectlen, 1)) == NULL)
	goto no_mem;
    }
    else
        pdu -> responding = NULL;

    if (status == PC_REJECTED) {
	if ((pdu -> reason = (struct type_PS_Rejection__reason *)
			malloc (sizeof (struct type_PS_Rejection__reason)))
		== NULL)
	    goto no_mem;

	pdu -> reason -> parm =
			    int_PS_Rejection__reason_rejected__by__responder;
    }
    else
	pdu -> reason = NULL;

    pdu -> user__data = data[0];

    pe = NULLPE;
    result = encode_PS_ConnectResponse__PDU (&pe, 1, 0, NULLCP, pdu);

    pdu -> reference = NULL;
    pdu -> user__data = NULLPE;
    free_PS_ConnectResponse__PDU (pdu);
    pdu = NULL;

    if (result == NOTOK) {
	(void) psaplose (pi, PC_CONGEST, NULLCP, "error encoding PDU: %s",
			 PY_pepy);
	goto out2;
    }

    PLOGP (psap2_log,PS_PDUs, pe, "ConnectResponse-PDU", 0);

    result = pe2ps (ps = pb -> pb_stream, pe);

    pe_free (pe);

    if (result == NOTOK) {
	result = pslose (pi, ps -> ps_errno);
	goto out1;
    }

    if (status == PC_ACCEPT)
	pb -> pb_flags |= PB_CONN;
    else
	freepblk (pb);

    return OK;

out2: ;
    if (pdu) {
	pdu -> reference = NULL;
	pdu -> user__data = NULLPE;
	free_PS_ConnectResponse__PDU (pdu);
    }
    if (pe)
	pe_free (pe);

out1: ;
    freepblk (pb);

    return NOTOK;
}

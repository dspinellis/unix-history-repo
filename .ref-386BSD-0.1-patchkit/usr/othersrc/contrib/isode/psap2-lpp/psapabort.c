/* psapabort.c - PPM: user abort */

#ifndef	lint
static char *rcsid = "$Header: /f/osi/psap2-lpp/RCS/psapabort.c,v 7.2 91/02/22 09:38:05 mrose Interim $";
#endif

/* 
 * $Header: /f/osi/psap2-lpp/RCS/psapabort.c,v 7.2 91/02/22 09:38:05 mrose Interim $
 *
 * Contributed by The Wollongong Group, Inc.
 *
 *
 * $Log:	psapabort.c,v $
 * Revision 7.2  91/02/22  09:38:05  mrose
 * Interim 6.8
 * 
 * Revision 7.1  90/07/01  21:05:23  mrose
 * pepsy
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

/*    P-U-ABORT.REQUEST */

int	PUAbortRequest (sd, data, ndata, pi)
int	sd;
PE     *data;
int	ndata;
struct PSAPindication *pi;
{
    SBV	    smask;
    int	    result;
    register struct psapblk *pb;
    PE	    pe;
    PS	    ps;
    register struct type_PS_Abort__PDU *pdu;

    toomuchP (data, ndata, NPDATA_PS, "abort");
    if (ndata > 0) {
	if ((pe = data[0]) -> pe_context != PCI_ACSE)
	    return psaplose (pi, PC_PARAMETER, NULLCP,
			     "wrong context for abort user data");
    }
    else
	pe = NULLPE;
    missingP (pi);

    smask = sigioblock ();

    if ((pb = findpblk (sd)) == NULL) {
	(void) sigiomask (smask);
	return psaplose (pi, PC_PARAMETER, NULLCP,
		"invalid presentation descriptor");
    }

    if ((pdu = (struct type_PS_Abort__PDU *) malloc (sizeof *pdu)) == NULL) {
        (void) psaplose (pi, PC_CONGEST, NULLCP, "out of memory");
	goto out;
    }
    pdu -> reference = pb -> pb_reliability == LOW_QUALITY ? pb -> pb_reference
							   : NULLRF;
    pdu -> user__data = pe;
    pdu -> reason = NULL;

    pe = NULLPE;
    result = encode_PS_Abort__PDU (&pe, 1, 0, NULLCP, pdu);

    pdu -> reference = NULL;
    pdu -> user__data = NULLPE;
    free_PS_Abort__PDU (pdu);

    if (result != NOTOK) {
	PLOGP (psap2_log,PS_PDUs, pe, "Abort-PDU", 0);

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

out: ;
    freepblk (pb);

    (void) sigiomask (smask);

    return result;
}

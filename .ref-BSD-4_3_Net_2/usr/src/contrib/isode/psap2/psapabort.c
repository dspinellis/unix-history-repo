/* psapabort.c - PPM: user abort */

#ifndef	lint
static char *rcsid = "$Header: /f/osi/psap2/RCS/psapabort.c,v 7.2 91/02/22 09:37:28 mrose Interim $";
#endif

/* 
 * $Header: /f/osi/psap2/RCS/psapabort.c,v 7.2 91/02/22 09:37:28 mrose Interim $
 *
 *
 * $Log:	psapabort.c,v $
 * Revision 7.2  91/02/22  09:37:28  mrose
 * Interim 6.8
 * 
 * Revision 7.1  90/07/01  21:04:56  mrose
 * pepsy
 * 
 * Revision 7.0  89/11/23  22:14:17  mrose
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
#include "tailor.h"

/*    P-U-ABORT.REQUEST */

int	PUAbortRequest (sd, data, ndata, pi)
int	sd;
PE     *data;
int	ndata;
struct PSAPindication *pi;
{
    SBV	    smask;
    int	    len,
	    result;
    char   *base;
    PE	    pe;
    register struct psapblk *pb;
    struct SSAPindication   sis;
    register struct SSAPabort  *sa = &sis.si_abort;
    register struct type_PS_ARU__PPDU *pdu;

    toomuchP (data, ndata, NPDATA, "abort");
    missingP (pi);

    smask = sigioblock ();

    if ((pb = findpblk (sd)) == NULL) {
	(void) sigiomask (smask);
	return psaplose (pi, PC_PARAMETER, NULLCP,
		"invalid presentation descriptor");
    }

    pe = NULLPE;
    base = NULLCP;
    result = NOTOK;
    if ((pdu = (struct type_PS_ARU__PPDU *) calloc (1, sizeof *pdu)) == NULL) {
no_mem: ;
	(void) psaplose (pi, PC_CONGEST, NULLCP, "out of memory");
	goto out2;
    }
    pdu -> offset = type_PS_ARU__PPDU_normal__mode;
    if ((pdu -> un.normal__mode = (struct element_PS_4 *)
				calloc (1, sizeof (struct element_PS_4)))
	    == NULL)
	goto no_mem;
    if (data && ndata > 0) {
	if (pb -> pb_ncontext > 0
	        && (pdu -> un.normal__mode -> context__list =
		    		silly_list (pb, pi)) == NULL)
	    goto out2;

	if ((pdu -> un.normal__mode -> user__data = info2ppdu (pb, pi,
							       data, ndata,
							       PPDU_NONE))
	    == NULL)
	goto out2;
    }

    if (encode_PS_ARU__PPDU (&pe, 1, 0, NULLCP, pdu) == NOTOK) {
	(void) psaplose (pi, PC_CONGEST, NULLCP, "error encoding PDU: %s",
			 PY_pepy);
	goto out2;
    }

    PLOGP (psap2_log,PS_ARU__PPDU, pe, "ARU-PPDU", 0);

    if (pe2ssdu (pe, &base, &len) == NOTOK)
	goto no_mem;

    if ((result = SUAbortRequest (pb -> pb_fd, base, len, &sis)) == NOTOK)
	if (SC_FATAL (sa -> sa_reason)) {
	    (void) ss2pslose (pb, pi, "SUAbortRequest", sa);
	    goto out2;
	}
	else {
	    (void) ss2pslose (NULLPB, pi, "SUAbortRequest", sa);	    
	    goto out1;
	}

    result = OK;
    pb -> pb_fd = NOTOK;

out2: ;
    freepblk (pb);

out1: ;
    if (pdu)
	free_PS_ARU__PPDU (pdu);
    if (pe)
	pe_free (pe);
    if (base)
	free (base);

    (void) sigiomask (smask);

    return result;
}

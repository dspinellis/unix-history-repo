/* acsapreleas2.c - ACPM: respond to release */

#ifndef	lint
static char *rcsid = "$Header: /f/osi/acsap/RCS/acsapreleas2.c,v 7.2 91/02/22 09:14:13 mrose Interim $";
#endif

/* 
 * $Header: /f/osi/acsap/RCS/acsapreleas2.c,v 7.2 91/02/22 09:14:13 mrose Interim $
 *
 *
 * $Log:	acsapreleas2.c,v $
 * Revision 7.2  91/02/22  09:14:13  mrose
 * Interim 6.8
 * 
 * Revision 7.1  90/07/01  21:02:04  mrose
 * pepsy
 * 
 * Revision 7.0  89/11/23  21:21:55  mrose
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
#include "ACS-types.h"
#define	ACSE
#include "acpkt.h"
#include "tailor.h"

/*    A-RELEASE.RESPONSE */

int	AcRelResponse (sd, status, reason, data, ndata, aci)
int	sd;
int	status,
	reason;
PE     *data;
int	ndata;
struct AcSAPindication *aci;
{
    SBV	    smask;
    int     code,
            result;
    register struct assocblk   *acb;
    PE	    pe;
    struct PSAPindication pis;
    register struct PSAPabort  *pa = &pis.pi_abort;
    register struct type_ACS_RLRE__apdu *pdu;

    switch (status) {
	case ACS_ACCEPT: 
	    code = SC_ACCEPT;
	    break;

	case ACS_REJECT: 
	    code = SC_REJECTED;
	    break;

	default: 
	    return acsaplose (aci, ACS_PARAMETER, NULLCP,
		    "invalid value for status parameter");
    }
    switch (reason) {
	case ACR_NORMAL: 
	case ACR_NOTFINISHED: 
	case ACR_USERDEFINED: 
	    break;

	default: 
	    return acsaplose (aci, ACS_PARAMETER, NULLCP,
		    "invalid value for reason parameter");
    }
    toomuchP (data, ndata, NACDATA, "release");
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

    smask = sigioblock ();

    acsapFsig (acb, sd);

    pe = NULLPE;
    if ((pdu = (struct type_ACS_RLRE__apdu *) calloc (1, sizeof *pdu))
	    == NULL) {
	(void) acsaplose (aci, ACS_CONGEST, NULLCP, "out of memory");
	goto out2;
    }
    pdu -> optionals |= opt_ACS_RLRE__apdu_reason;
    pdu -> reason = reason;
    if (data
	    && ndata > 0
	    && (pdu -> user__information = info2apdu (acb, aci, data, ndata))
		    == NULL)
	goto out2;

    result = encode_ACS_RLRE__apdu (&pe, 1, 0, NULLCP, pdu);

    free_ACS_RLRE__apdu (pdu);
    pdu = NULL;

    if (result == NOTOK) {
	(void) acsaplose (aci, ACS_CONGEST, NULLCP, "error encoding PDU: %s",
			  PY_pepy);
	goto out2;
    }
    pe -> pe_context = acb -> acb_id;

    PLOGP (acsap_log,ACS_ACSE__apdu, pe, "RLRE-apdu", 0);

    if ((result = PRelResponse (acb -> acb_fd, code, &pe, 1, &pis)) == NOTOK) {
	(void) ps2acslose (acb, aci, "PRelResponse", pa);
	if (PC_FATAL (pa -> pa_reason))
	    goto out2;
	else
	    goto out1;
    }

    if (status == ACS_ACCEPT)
	acb -> acb_fd = NOTOK;
    else
	acb -> acb_flags &= ~ACB_FINN;

    result = OK;

out2: ;
    if (result == NOTOK || status == ACS_ACCEPT)
	freeacblk (acb);
out1: ;
    if (pe)
	pe_free (pe);
    if (pdu)
	free_ACS_RLRE__apdu (pdu);

    (void) sigiomask (smask);

    return result;
}

/* acsaplose.c - ACPM: you lose */

#ifndef	lint
static char *rcsid = "$Header: /f/osi/acsap/RCS/acsaplose.c,v 7.2 91/02/22 09:14:11 mrose Interim $";
#endif

/* 
 * $Header: /f/osi/acsap/RCS/acsaplose.c,v 7.2 91/02/22 09:14:11 mrose Interim $
 *
 *
 * $Log:	acsaplose.c,v $
 * Revision 7.2  91/02/22  09:14:11  mrose
 * Interim 6.8
 * 
 * Revision 7.1  90/07/01  21:01:58  mrose
 * pepsy
 * 
 * Revision 7.0  89/11/23  21:21:52  mrose
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
#include <varargs.h>
#include "ACS-types.h"
#define	ACSE
#include "acpkt.h"
#include "tailor.h"

/*  */

#ifndef	lint
int	acpktlose (va_alist)
va_dcl
{
    int	    reason,
    	    result;
    PE	    pe;
    register struct assocblk *acb;
    register struct AcSAPindication *aci;
    struct PSAPindication   pis;
    struct type_ACS_ABRT__apdu pdus;
    register struct type_ACS_ABRT__apdu *pdu = &pdus;
    va_list ap;

    va_start (ap);

    acb = va_arg (ap, struct assocblk *);
    aci = va_arg (ap, struct AcSAPindication *);
    reason = va_arg (ap, int);

    result = _acsaplose (aci, reason, ap);

    va_end (ap);

    if (acb == NULLACB || acb -> acb_fd == NOTOK)
	return result;

    if (acb -> acb_sversion == 1) {
	if (PUAbortRequest (acb -> acb_fd, NULLPEP, 0, &pis) != NOTOK)
	    acb -> acb_fd = NOTOK;

	return result;
    }
    
    pdu -> abort__source = int_ACS_abort__source_acse__service__provider;
    pdu -> user__information = NULL;

    pe = NULLPE;
    if (encode_ACS_ABRT__apdu (&pe, 1, 0, NULLCP, pdu) != NOTOK) {
	pe -> pe_context = acb -> acb_id;

	PLOGP (acsap_log,ACS_ACSE__apdu, pe, "ABRT-apdu", 0);

	if (PUAbortRequest (acb -> acb_fd, &pe, 1, &pis) != NOTOK)
	    acb -> acb_fd = NOTOK;
    }
    if (pe)
	pe_free (pe);

    return result;
}
#else
/* VARARGS5 */

int	acpktlose (acb, aci, reason, what, fmt)
struct assocblk *acb;
struct AcSAPindication *aci;
int	reason;
char   *what,
       *fmt;
{
    return acpktlose (acb, aci, reason, what, fmt);
}
#endif

/*  */

#ifndef	lint
int	acsaplose (va_alist)
va_dcl
{
    int	    reason,
	    result;
    struct AcSAPindication *aci;
    va_list ap;

    va_start (ap);

    aci = va_arg (ap, struct AcSAPindication *);
    reason = va_arg (ap, int);

    result = _acsaplose (aci, reason, ap);

    va_end (ap);

    return result;
}
#else
/* VARARGS4 */

int	acsaplose (aci, reason, what, fmt)
struct AcSAPindication *aci;
int	reason;
char   *what,
       *fmt;
{
    return acsaplose (aci, reason, what, fmt);
}
#endif

/*  */

#ifndef	lint
static int  _acsaplose (aci, reason, ap)  /* what, fmt, args ... */
register struct AcSAPindication *aci;
int     reason;
va_list	ap;
{
    register char  *bp;
    char    buffer[BUFSIZ];
    register struct AcSAPabort *aca;

    if (aci) {
	bzero ((char *) aci, sizeof *aci);
	aci -> aci_type = ACI_ABORT;
	aca = &aci -> aci_abort;

	asprintf (bp = buffer, ap);
	bp += strlen (bp);

	aca -> aca_source = ACA_LOCAL;
	aca -> aca_reason = reason;
	copyAcSAPdata (buffer, bp - buffer, aca);
    }

    return NOTOK;
}
#endif

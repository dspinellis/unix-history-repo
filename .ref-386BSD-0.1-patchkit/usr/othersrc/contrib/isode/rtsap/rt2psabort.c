/* rt2psabort.c - RTPM: user abort */

#ifndef	lint
static char *rcsid = "$Header: /f/osi/rtsap/RCS/rt2psabort.c,v 7.4 91/02/22 09:42:18 mrose Interim $";
#endif

/* 
 * $Header: /f/osi/rtsap/RCS/rt2psabort.c,v 7.4 91/02/22 09:42:18 mrose Interim $
 *
 *
 * $Log:	rt2psabort.c,v $
 * Revision 7.4  91/02/22  09:42:18  mrose
 * Interim 6.8
 * 
 * Revision 7.3  90/10/23  20:44:02  mrose
 * update
 * 
 * Revision 7.2  90/07/27  08:47:43  mrose
 * update
 * 
 * Revision 7.1  90/07/01  21:06:51  mrose
 * pepsy
 * 
 * Revision 6.0  89/03/18  23:43:08  mrose
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
#include "logger.h"

/*    RT-U-ABORT.REQUEST */

int	RtUAbortRequest (sd, data, rti)
int	sd;
PE	data;
struct RtSAPindication *rti;
{
    SBV	    smask;
    int	    result;
    register struct assocblk *acb;

    missingP (rti);

    smask = sigioblock ();

    rtsapPsig (acb, sd);

    result = RtUAbortRequestAux (acb, data, rti);

    (void) sigiomask (smask);

    return result;
}

/*  */

static int  RtUAbortRequestAux (acb, data, rti)
register struct assocblk *acb;
PE	data;
register struct RtSAPindication *rti;
{
    int	    result;
    PE	    pe,
	    p;
    struct AcSAPindication acis;
    register struct AcSAPindication *aci = &acis;
    register struct AcSAPabort *aca = &aci -> aci_abort;

    if (!(acb -> acb_flags & ACB_ACS))
	return rtsaplose (rti, RTS_OPERATION, NULLCP,
		    "not an association descriptor for RTS");
/* begin RTAB APDU */
    if ((pe = pe_alloc (PE_CLASS_UNIV, PE_FORM_CONS, 22)) == NULLPE
	    || set_add (pe, num2prim ((integer) ABORT_USER, PE_CLASS_CONT,
				      RTAB_REASON)) == NOTOK
	    || (data
		    && (set_add (pe, p = pe_alloc (PE_CLASS_CONT, PE_FORM_CONS,
				RTAB_USERDATA)) == NOTOK
			    || set_add (p, data) == NOTOK))) {
	result = rtsaplose (rti, RTS_CONGEST, NULLCP, "out of memory");
	(void) AcUAbortRequest (acb -> acb_fd, NULLPEP, 0, aci);
	goto out;
    }
    pe -> pe_context = acb -> acb_rtsid;
/* end RTAB APDU */

    PLOGP (rtsap_log,RTS_RTSE__apdus, pe, "RTABapdu", 0);

    if ((result = AcUAbortRequest (acb -> acb_fd, &pe, 1, aci)) == NOTOK)
	(void) acs2rtslose (acb, rti, "AcUAbortRequest", aca);
    else
	result = OK;

out: ;
    if (pe) {
	if (data)
	    (void) pe_extract (pe, data);
	pe_free (pe);
    }

    return result;
}

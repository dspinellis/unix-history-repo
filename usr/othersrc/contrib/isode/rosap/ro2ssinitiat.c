/* ro2ssinitiat.c - initiator */

#ifndef	lint
static char *rcsid = "$Header: /f/osi/rosap/RCS/ro2ssinitiat.c,v 7.3 91/02/22 09:41:18 mrose Interim $";
#endif

/* 
 * $Header: /f/osi/rosap/RCS/ro2ssinitiat.c,v 7.3 91/02/22 09:41:18 mrose Interim $
 *
 * Based on an TCP-based implementation by George Michaelson of University
 * College London.
 *
 *
 * $Log:	ro2ssinitiat.c,v $
 * Revision 7.3  91/02/22  09:41:18  mrose
 * Interim 6.8
 * 
 * Revision 7.2  90/11/05  13:33:07  mrose
 * update
 * 
 * Revision 7.1  90/07/01  21:05:52  mrose
 * pepsy
 * 
 * Revision 6.1  89/06/24  00:55:54  mrose
 * reason
 * 
 * Revision 6.0  89/03/18  23:42:16  mrose
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
#include "../acsap/OACS-types.h"
#include "ropkt.h"
#include "isoservent.h"
#include "tailor.h"

/*    RO-BEGIN.REQUEST */

int	RoBeginRequest (called, data, roc, roi)
struct RoSAPaddr *called;
PE	data;
struct RoSAPconnect *roc;
struct RoSAPindication *roi;
{
    SBV     smask;
    int     result;

    isodetailor (NULLCP, 0);

    missingP (called);
    missingP (roc);
    bzero ((char *) roc, sizeof *roc);
    missingP (roi);

    smask = sigioblock ();

    result = RoBeginRequestAux (called, data, roc, roi);

    (void) sigiomask (smask);

    return result;
}

/*  */

static int  RoBeginRequestAux (called, data, roc, roi)
struct RoSAPaddr *called;
PE	data;
struct RoSAPconnect *roc;
struct RoSAPindication *roi;
{
    int	    len,
	    result,
	    settings;
    char   *base;
#ifdef	notdef
    register struct isoservent *is;
#endif
    PE	pe;
    register struct assocblk *acb;
    struct SSAPref ref;
    struct SSAPconnect scs;
    register struct SSAPconnect *sc = &scs;
    struct SSAPindication sis;
    register struct SSAPindication *si = &sis;
    register struct SSAPabort *sa = &si -> si_abort;
    struct type_OACS_PConnect pcnnct;
    struct type_OACS_DataTransferSyntax dts;
    struct member_OACS_1 udata;
    struct type_OACS_ConnectionData condata;
    struct type_OACS_PAccept	*paccpt;
    struct type_OACS_PRefuse pref;

    if ((acb = newacblk ()) == NULL)
	return rosaplose (roi, ROS_CONGEST, NULLCP, "out of memory");

    pcnnct.member_OACS_0 = &dts;
    dts.parm = int_OACS_DataTransferSyntax_x409;
    pcnnct.pUserData = &udata;
    udata.optionals = 0;
    udata.member_OACS_2 = &condata;
    condata.offset = type_OACS_ConnectionData_open;
    if (data)
	condata.un.open = data;
    else
	/* Seems to be needed to make the responder happy. */
	condata.un.open = pe_alloc (PE_CLASS_UNIV, PE_FORM_PRIM, PE_PRIM_NULL);
    udata.applicationProtocol = (int) ntohs (called -> roa_port);
    if (encode_OACS_PConnect (&pe, 1, 0, NULLCP, &pcnnct) == NOTOK) {
no_mem: ;
	result = rosaplose (roi, ROS_CONGEST, NULLCP, "out of memory");
	goto out1;
    }

    PLOGP (rosap_log,OACS_PConnect, pe, "PConnect", 0);

    if (pe2ssdu (pe, &base, &len) == NOTOK)
	goto no_mem;

    if (data)
	(void) pe_extract (pe, data), data = NULLPE;
    pe_free (pe);
    pe = NULLPE;

#ifdef	notdef		/* SEK doesn't like this */
    if (called -> roa_addr.sa_selectlen == 0) {
	if ((is = getisoserventbyname ("ros", "ssap")) == NULL) {
	    result = rosaplose (roi, ROS_ADDRESS, NULLCP,
			"ssap/ros: unknown entity");
	    goto out2;
	}
	if (is -> is_selectlen > SSSIZE) {	/* XXX */
	    result = rosaplose (roi, ROS_ADDRESS, NULLCP,
			"ssap/ros: selector too long (%d octets)",
			is -> is_selectlen);
	    goto out2;
	}
	bcopy (is -> is_selector, called -> roa_addr.sa_selector,
	    called -> roa_addr.sa_selectlen = is -> is_selectlen);
    }
#endif

    acb -> acb_requirements = SR_BCSUBSET;
    settings = 0;
#define dotoken(requires,shift,bit,type) \
{ \
    if (acb -> acb_requirements & requires) \
	settings |= ST_INIT_VALUE << shift; \
}
    dotokens ();
#undef	dotoken

    bzero ((char *) &ref, sizeof ref);	/* ECMA says don't encode this yet */
    if (SConnRequest (&ref, NULLSA, &called -> roa_addr,
		acb -> acb_requirements, settings, SERIAL_NONE, base, len,
		NULLQOS, sc, si) == NOTOK) {
	result = ss2roslose (NULLACB, roi, "SConnRequest", sa);
	goto out2;
    }
    free (base);

    bzero ((char *) roc, sizeof *roc);

    if (sc -> sc_result == SC_ACCEPT) {
	acb -> acb_fd = sc -> sc_sd;
	acb -> acb_uabort = SUAbortRequest;
    }
    else
        if (sc -> sc_result == SC_ABORT) {
	    acb -> acb_fd = NOTOK;

	    (void) ss2rosabort (acb, sa, roi);

	    roc -> roc_sd = NOTOK;
	    roc -> roc_result = ROS_ABORTED;

	    return OK;
	}

    if ((pe = ssdu2pe (sc -> sc_data, sc -> sc_cc, NULLCP, &result))
	    == NULLPE) {
	if (sc -> sc_result != SC_ACCEPT) {
	    bzero ((char *) sa, sizeof *sa);
	    sa -> sa_reason = sc -> sc_result;
	    acb -> acb_fd = NOTOK;
	    (void) ss2roslose (acb, roi, "SConnRequest(pseudo)", sa);

	    roc -> roc_sd = NOTOK;
	    roc -> roc_result = roi -> roi_preject.rop_reason;

	    result = OK;
	}
	else
	    result = ropktlose (acb, roi, result != PS_ERR_NMEM ? ROS_PROTOCOL
		    : ROS_CONGEST, NULLCP, "%s", ps_error (result));
	goto out1;
    }

    SCFREE (sc);

    if (sc -> sc_result != SC_ACCEPT) {
	if (parse_OACS_PRefuse (pe, 1, NULLIP, NULLVP, &pref) == NOTOK) {
	    result = pylose ();
	    goto out1;
	}

	PLOGP (rosap_log,OACS_PRefuse, pe, "PRefuse", 1);

	pe_free (pe);
	
	freeacblk (acb);

	roc -> roc_sd = NOTOK;
	switch (pref.parm) {
	    case REFUSE_BUSY: 
		roc -> roc_result = ROS_BUSY;
		break;

	    case REFUSE_VALIDATE: 
		roc -> roc_result = ROS_VALIDATE;
		break;

	    default: 
		roc -> roc_result = ROS_PROTOCOL;
		break;
	}

	return OK;
    }

    acb -> acb_flags = ACB_CONN | ACB_ROS | ACB_INIT;
    (void) RoSService (acb, roi);
    if (!((acb -> acb_requirements = sc -> sc_requirements) & SR_DUPLEX)
	    && !(acb -> acb_requirements & SR_HALFDUPLEX)) {
	result = ropktlose (acb, roi, ROS_PROTOCOL, NULLCP,
		    "desired session requirements denied");
	goto out1;
    }
    if (acb -> acb_requirements & SR_HALFDUPLEX)
	acb -> acb_flags |= ACB_TURN;	/* it it is there we must have it */

    if (parse_OACS_PAccept (pe, 1, NULLIP, NULLVP, &paccpt) == NOTOK) {
	result = pylose ();
	goto out1;
    }

    PLOGP (rosap_log,OACS_PAccept, pe, "PAccept", 1);

    roc -> roc_sd = acb -> acb_fd;
    roc -> roc_result = ROS_ACCEPT;
    if (paccpt -> pUserData -> member_OACS_5->offset
	    == type_OACS_ConnectionData_open)
	roc -> roc_data = pe_expunge (pe, paccpt -> pUserData
						 -> member_OACS_5 -> un.open);
    else
	roc -> roc_data = NULLPE;
	
    free_OACS_PAccept (paccpt);
    return OK;

out2: ;
    free (base);

out1: ;
    SCFREE (sc);
    if (pe) {
	if (data)
	    (void) pe_extract (pe, data);
	pe_free (pe);
    }
    freeacblk (acb);

    return result;
}

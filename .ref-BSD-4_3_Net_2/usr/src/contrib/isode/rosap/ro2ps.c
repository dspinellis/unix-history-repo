/* ro2ps.c - ROPM: PSAP interface */

#ifndef	lint
static char *rcsid = "$Header: /f/osi/rosap/RCS/ro2ps.c,v 7.5 91/02/22 09:41:10 mrose Interim $";
#endif

/* 
 * $Header: /f/osi/rosap/RCS/ro2ps.c,v 7.5 91/02/22 09:41:10 mrose Interim $
 *
 * Based on an TCP-based implementation by George Michaelson of University
 * College London.
 *
 *
 * $Log:	ro2ps.c,v $
 * Revision 7.5  91/02/22  09:41:10  mrose
 * Interim 6.8
 * 
 * Revision 7.4  90/11/21  11:31:17  mrose
 * sun
 * 
 * Revision 7.3  90/11/05  13:33:05  mrose
 * update
 * 
 * Revision 7.2  90/10/17  11:55:53  mrose
 * sync
 * 
 * Revision 7.1  90/07/01  21:05:43  mrose
 * pepsy
 * 
 * Revision 6.0  89/03/18  23:42:08  mrose
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
#include "ROS-types.h"
#include "ropkt.h"
#include "tailor.h"

/*    DATA */

int	acslose ();

int	pslose ();
int	psDATAser (), psTOKENser (), psSYNCser (), psACTIVITYser (),
	psREPORTser (), psFINISHser (), psABORTser ();

/*    bind underlying service */

int	RoPService (acb, roi)
register struct assocblk   *acb;
struct RoSAPindication *roi;
{
    if (!(acb -> acb_flags & ACB_ACS) || (acb -> acb_flags & ACB_RTS))
	return rosaplose (roi, ROS_OPERATION, NULLCP,
		"not an association descriptor for ROS on presentation");

    acb -> acb_putosdu = ro2pswrite;
    acb -> acb_rowaitrequest = ro2pswait;
    acb -> acb_ready = NULLIFP;
    acb -> acb_rosetindications = ro2psasync;
    acb -> acb_roselectmask = ro2psmask;
    acb -> acb_ropktlose = NULLIFP;

    return OK;
}

/*    define vectors for INDICATION events */

#define	e(i)	(indication ? (i) : NULLIFP)


/* ARGSUSED */

int	ro2psasync (acb, indication, roi)
register struct assocblk   *acb;
IFP	indication;
struct RoSAPindication *roi;
{
    struct PSAPindication   pis;
    register struct PSAPabort  *pa = &pis.pi_abort;

    if (acb -> acb_rosindication = indication)
	acb -> acb_flags |= ACB_ASYN;
    else
	acb -> acb_flags &= ~ACB_ASYN;

    if (PSetIndications (acb -> acb_fd, e (psDATAser), e (psTOKENser),
		e (psSYNCser), e (psACTIVITYser), e (psREPORTser),
		e (psFINISHser), e (psABORTser), &pis) == NOTOK) {
	acb -> acb_flags &= ~ACB_ASYN;
	switch (pa -> pa_reason) {
	    case PC_WAITING: 
		return rosaplose (roi, ROS_WAITING, NULLCP, NULLCP);

	    default: 
		(void) pslose (acb, roi, "PSetIndications", pa);
		freeacblk (acb);
		return NOTOK;
	}
    }

    return OK;
}

#undef	e

/*    map association descriptors for select() */

/* ARGSUSED */

int	ro2psmask (acb, mask, nfds, roi)
register struct assocblk   *acb;
fd_set *mask;
int    *nfds;
struct RoSAPindication *roi;
{
    struct PSAPindication   pis;
    struct PSAPabort   *pa = &pis.pi_abort;

    if (PSelectMask (acb -> acb_fd, mask, nfds, &pis) == NOTOK)
	switch (pa -> pa_reason) {
	    case PC_WAITING: 
		return rosaplose (roi, ROS_WAITING, NULLCP, NULLCP);

	    default: 
		(void) pslose (acb, roi, "PSelectMask", pa);
		freeacblk (acb);
		return NOTOK;
	}

    return OK;
}

/*    AcSAP interface */

static int acslose (acb, roi, event, aca)
register struct assocblk *acb;
register struct RoSAPindication *roi;
char   *event;
register struct AcSAPabort *aca;
{
    int     reason;
    char   *cp,
            buffer[BUFSIZ];

    if (event)
	SLOG (rosap_log, LLOG_EXCEPTIONS, NULLCP,
	      (aca -> aca_cc > 0 ? "%s: %s [%*.*s]": "%s: %s", event,
	       AcErrString (aca -> aca_reason), aca -> aca_cc, aca -> aca_cc,
	       aca -> aca_data));

    cp = "";
    switch (aca -> aca_reason) {
	case ACS_ADDRESS: 
	    reason = ROS_ADDRESS;
	    break;

	case ACS_REFUSED:
	    reason = ROS_REFUSED;
	    break;

	case ACS_CONGEST: 
	    reason = ROS_CONGEST;
	    break;

	default: 
	    (void) sprintf (cp = buffer, " (%s at association control)",
		    AcErrString (aca -> aca_reason));
	case ACS_PRESENTATION:
	    reason = ROS_ACS;
	    break;
    }

    if (aca -> aca_cc > 0)
	return ropktlose (acb, roi, reason, NULLCP, "%*.*s%s",
		aca -> aca_cc, aca -> aca_cc, aca -> aca_data, cp);
    else
	return ropktlose (acb, roi, reason, NULLCP, "%s", cp);
}

/*    PSAP interface */

int	ro2pswait (acb, invokeID, secs, roi)
register struct assocblk *acb;
int    *invokeID,
	secs;
register struct RoSAPindication *roi;
{
    int     result;
    struct PSAPdata pxs;
    register struct PSAPdata   *px = &pxs;
    struct PSAPindication   pis;
    register struct PSAPindication *pi = &pis;

    for (;;) {
	switch (result = PReadRequest (acb -> acb_fd, px, secs, pi)) {
	    case NOTOK: 
		return doPSabort (acb, &pi -> pi_abort, roi);

	    case OK: 
		if ((result = doPSdata (acb, invokeID, px, roi)) != OK)
		    return (result != DONE ? result : OK);
		continue;

	    case DONE: 
		switch (pi -> pi_type) {
		    case PI_TOKEN: 
			if (doPStokens (acb, &pi -> pi_token, roi) == NOTOK)
			    return NOTOK;
			continue;

		    case PI_SYNC: 
			if (doPSsync (acb, &pi -> pi_sync, roi) == NOTOK)
			    return NOTOK;
			continue;

		    case PI_ACTIVITY: 
			if (doPSactivity (acb, &pi -> pi_activity, roi) == NOTOK)
			    return NOTOK;
			continue;

		    case PI_REPORT: 
			if (doPSreport (acb, &pi -> pi_report, roi) == NOTOK)
			    return NOTOK;
			continue;

		    case PI_FINISH: 
			if (doPSfinish (acb, &pi -> pi_finish, roi) == NOTOK)
			    return NOTOK;
			return DONE;

		    default: 
			(void) ropktlose (acb, roi, ROS_PROTOCOL, NULLCP,
				"unknown indication (0x%x) from presentation",
				pi -> pi_type);
			break;
		}
		break;

	    default: 
		(void) ropktlose (acb, roi, ROS_PROTOCOL, NULLCP,
			"unexpected return from PReadRequest=%d", result);
		break;
	}
	break;
    }

    freeacblk (acb);

    return NOTOK;
}

/*  */

/* ARGSUSED */

int	ro2pswrite (acb, pe, fe, priority, roi)
register struct assocblk *acb;
PE	pe,
	fe;
int	priority;
struct RoSAPindication *roi;
{
    int	    result;
    struct PSAPindication   pis;
    register struct PSAPabort  *pa = &pis.pi_abort;

    pe -> pe_context = acb -> acb_rosid;

    PLOGP (rosap_log,ROS_ROSEapdus, pe, "ROSEapdus", 0);

    if ((result = PDataRequest (acb -> acb_fd, &pe, 1, &pis)) == NOTOK) {
	(void) pslose (acb, roi, "PDataRequest", pa);	
	freeacblk (acb);
    }
    
    if (fe)
	    (void) pe_extract (pe, fe);

    pe_free (pe);

    return result;
}

/*  */

static int  doPSdata (acb, invokeID, px, roi)
register struct assocblk   *acb;
int    *invokeID;
register struct PSAPdata *px;
struct RoSAPindication *roi;
{
    register PE	    pe;

    if (px -> px_type != SX_NORMAL) {
	(void) ropktlose (acb, roi, ROS_PROTOCOL, NULLCP,
		"unexpected data indication (0x%x)", px -> px_type);
	PXFREE (px);

	freeacblk (acb);
	return NOTOK;
    }

    pe = px -> px_info[0], px -> px_info[0] = NULLPE;
    PXFREE (px);

    return acb2osdu (acb, invokeID, pe, roi);
}

/*  */

static int  doPStokens (acb, pt, roi)
register struct assocblk   *acb;
register struct PSAPtoken *pt;
struct RoSAPindication *roi;
{
    (void) ropktlose (acb, roi, ROS_PROTOCOL, NULLCP,
	    "unexpected token indication (0x%x)", pt -> pt_type);
    PTFREE (pt);

    freeacblk (acb);
    return NOTOK;
}

/*  */

static int  doPSsync (acb, pn, roi)
register struct assocblk   *acb;
register struct PSAPsync *pn;
struct RoSAPindication *roi;
{
    (void) ropktlose (acb, roi, ROS_PROTOCOL, NULLCP,
	    "unexpected sync indication (0x%x)", pn -> pn_type);
    PNFREE (pn);

    freeacblk (acb);
    return NOTOK;
}

/*  */

static int  doPSactivity (acb, pv, roi)
register struct assocblk   *acb;
register struct PSAPactivity *pv;
struct RoSAPindication *roi;
{
    (void) ropktlose (acb, roi, ROS_PROTOCOL, NULLCP,
	    "unexpected activity indication (0x%x)", pv -> pv_type);
    PVFREE (pv);

    freeacblk (acb);
    return NOTOK;
}

/*  */

static int  doPSreport (acb, pp, roi)
register struct assocblk   *acb;
register struct PSAPreport *pp;
struct RoSAPindication *roi;
{
    (void) ropktlose (acb, roi, ROS_PROTOCOL, NULLCP,
	    "unexpected exception report indication (0x%x)", pp -> pp_peer);
    PPFREE (pp);

    freeacblk (acb);
    return NOTOK;
}

/*  */

/* ARGSUSED */

static int  doPSfinish (acb, pf, roi)
register struct assocblk   *acb;
struct PSAPfinish *pf;
struct RoSAPindication *roi;
{
    struct AcSAPindication acis;
    register struct AcSAPabort *aca = &acis.aci_abort;

    if (acb -> acb_flags & ACB_INIT) {
	(void) ropktlose (acb, roi, ROS_PROTOCOL, NULLCP,
		"association management botched");
	PFFREE (pf);
	freeacblk (acb);
	return NOTOK;
    }

    roi -> roi_type = ROI_FINISH;
    {
	register struct AcSAPfinish   *acf = &roi -> roi_finish;

	if (AcFINISHser (acb -> acb_fd, pf, &acis) == NOTOK)
	    return acslose (acb, roi, "AcFINISHser", aca);

	*acf = acis.aci_finish;	/* struct copy */
    }

    return DONE;
}

/*  */

static int  doPSabort (acb, pa, roi)
register struct assocblk   *acb;
register struct PSAPabort *pa;
struct RoSAPindication *roi;
{
    struct AcSAPindication acis;
    register struct AcSAPabort *aca = &acis.aci_abort;

    if (!pa -> pa_peer && pa -> pa_reason == PC_TIMER)
	return rosaplose (roi, ROS_TIMER, NULLCP, NULLCP);

    if (AcABORTser (acb -> acb_fd, pa, &acis) == NOTOK) {
	(void) acslose (acb, roi, "AcABORTser", aca);
	goto out;
    }

    if (aca -> aca_source != ACA_USER)
	(void) acslose (acb, roi, NULLCP, aca);
    else
	(void) rosaplose (roi, ROS_ABORTED, NULLCP, NULLCP);

    /* XXX: perhaps should pass data up? */
    ACAFREE (aca);

out: ;
    acb -> acb_fd = NOTOK;
    freeacblk (acb);

    return NOTOK;
}

/*  */

static int  psDATAser (sd, px)
int	sd;
register struct PSAPdata *px;
{
    IFP	    handler;
    register struct assocblk   *acb;
    struct RoSAPindication  rois;
    register struct RoSAPindication *roi = &rois;

    if ((acb = findacblk (sd)) == NULL)
	return;
    handler = acb -> acb_rosindication;

    if (doPSdata (acb, NULLIP, px, roi) != OK)
	(*handler) (sd, roi);
}

/*  */

static int  psTOKENser (sd, pt)
int	sd;
register struct PSAPtoken *pt;
{
    IFP	    handler;
    register struct assocblk   *acb;
    struct RoSAPindication  rois;
    register struct RoSAPindication *roi = &rois;

    if ((acb = findacblk (sd)) == NULL)
	return;
    handler = acb -> acb_rosindication;

    if (doPStokens (acb, pt, roi) != OK)
	(*handler) (sd, roi);
}

/*  */

static int  psSYNCser (sd, pn)
int	sd;
register struct PSAPsync *pn;
{
    IFP	    handler;
    register struct assocblk   *acb;
    struct RoSAPindication  rois;
    register struct RoSAPindication *roi = &rois;

    if ((acb = findacblk (sd)) == NULL)
	return;
    handler = acb -> acb_rosindication;

    if (doPSsync (acb, pn, roi) != OK)
	(*handler) (sd, roi);
}

/*  */

static int  psACTIVITYser (sd, pv)
int	sd;
register struct PSAPactivity *pv;
{
    IFP	    handler;
    register struct assocblk   *acb;
    struct RoSAPindication  rois;
    register struct RoSAPindication *roi = &rois;

    if ((acb = findacblk (sd)) == NULL)
	return;
    handler = acb -> acb_rosindication;

    if (doPSactivity (acb, pv, roi) != OK)
	(*handler) (sd, roi);
}

/*  */

static int  psREPORTser (sd, pp)
int	sd;
register struct PSAPreport *pp;
{
    IFP	    handler;
    register struct assocblk   *acb;
    struct RoSAPindication  rois;
    register struct RoSAPindication *roi = &rois;

    if ((acb = findacblk (sd)) == NULL)
	return;
    handler = acb -> acb_rosindication;

    if (doPSreport (acb, pp, roi) != OK)
	(*handler) (sd, roi);
}

/*  */

static int  psFINISHser (sd, pf)
int	sd;
struct PSAPfinish *pf;
{
    IFP	    handler;
    register struct assocblk   *acb;
    struct RoSAPindication  rois;
    register struct RoSAPindication *roi = &rois;

    if ((acb = findacblk (sd)) == NULL)
	return;
    handler = acb -> acb_rosindication;

    (void) doPSfinish (acb, pf, roi);

    (*handler) (sd, roi);
}

/*  */

static int  psABORTser (sd, pa)
int	sd;
register struct PSAPabort *pa;
{
    IFP	    handler;
    register struct assocblk   *acb;
    struct RoSAPindication  rois;
    register struct RoSAPindication *roi = &rois;

    if ((acb = findacblk (sd)) == NULL)
	return;
    handler = acb -> acb_rosindication;

    (void) doPSabort (acb, pa, roi);

    (*handler) (sd, roi);
}

/*  */

static int pslose (acb, roi, event, pa)
register struct assocblk *acb;
register struct RoSAPindication *roi;
char   *event;
register struct PSAPabort *pa;
{
    int     reason;
    char   *cp,
            buffer[BUFSIZ];

    if (event)
	SLOG (rosap_log, LLOG_EXCEPTIONS, NULLCP,
	      (pa -> pa_cc > 0 ? "%s: %s [%*.*s]": "%s: %s", event,
	       PErrString (pa -> pa_reason), pa -> pa_cc, pa -> pa_cc,
	       pa -> pa_data));

    cp = "";
    switch (pa -> pa_reason) {
	case PC_ADDRESS: 
	    reason = ROS_ADDRESS;
	    break;

	case PC_REFUSED:
	    reason = ROS_REFUSED;
	    break;

	case PC_CONGEST: 
	    reason = ROS_CONGEST;
	    break;

	default: 
	    (void) sprintf (cp = buffer, " (%s at presentation)",
		    PErrString (pa -> pa_reason));
	case PC_SESSION:
	    reason = ROS_PRESENTATION;
	    break;
    }

    if (pa -> pa_cc > 0)
	return ropktlose (acb, roi, reason, NULLCP, "%*.*s%s",
		pa -> pa_cc, pa -> pa_cc, pa -> pa_data, cp);
    else
	return ropktlose (acb, roi, reason, NULLCP, "%s", *cp ? cp + 1 : cp);
}

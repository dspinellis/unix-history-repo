/* ro2ss.c - ROPM: SSAP interface */

#ifndef	lint
static char *rcsid = "$Header: /f/osi/rosap/RCS/ro2ss.c,v 7.5 91/02/22 09:41:14 mrose Interim $";
#endif

/* 
 * $Header: /f/osi/rosap/RCS/ro2ss.c,v 7.5 91/02/22 09:41:14 mrose Interim $
 *
 * Based on an TCP-based implementation by George Michaelson of University
 * College London.
 *
 *
 * $Log:	ro2ss.c,v $
 * Revision 7.5  91/02/22  09:41:14  mrose
 * Interim 6.8
 * 
 * Revision 7.4  91/01/24  14:50:48  mrose
 * update
 * 
 * Revision 7.3  90/11/21  11:31:23  mrose
 * sun
 * 
 * Revision 7.2  90/10/23  20:44:19  mrose
 * update
 * 
 * Revision 7.1  90/07/01  21:05:48  mrose
 * pepsy
 * 
 * Revision 6.0  89/03/18  23:42:14  mrose
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
#include "../acsap/OACS-types.h"
#include "ropkt.h"
#include "tailor.h"

/*    DATA */

#define	doSSabort	ss2rosabort


int	ssDATAser (), ssTOKENser (), ssSYNCser (), ssACTIVITYser (),
	ssREPORTser (), ssFINISHser (), ssABORTser ();

/*    local stub routine for psap/qbuf2pe */

static        PE      qbuf2pe_local (qb, len, result)
register struct qbuf *qb;
int     len;
int    *result;
{
    return(qbuf2pe(qb, len, result));
}

/*    bind underlying service */

int	RoSService (acb, roi)
register struct assocblk   *acb;
struct RoSAPindication *roi;
{
    if (acb -> acb_flags & (ACB_ACS | ACB_RTS))
	return rosaplose (roi, ROS_OPERATION, NULLCP,
		"not an association descriptor for ROS on session");

    acb -> acb_putosdu = ro2sswrite;
    acb -> acb_rowaitrequest = ro2sswait;
    acb -> acb_getosdu = qbuf2pe_local;
    acb -> acb_ready = ro2ssready;
    acb -> acb_rosetindications = ro2ssasync;
    acb -> acb_roselectmask = ro2ssmask;
    acb -> acb_ropktlose = ro2sslose;

    return OK;
}

/*    define vectors for INDICATION events */

#define	e(i)	(indication ? (i) : NULLIFP)


/* ARGSUSED */

int	ro2ssasync (acb, indication, roi)
register struct assocblk   *acb;
IFP	indication;
struct RoSAPindication *roi;
{
    struct SSAPindication   sis;
    register struct SSAPabort  *sa = &sis.si_abort;

    if (acb -> acb_rosindication = indication)
	acb -> acb_flags |= ACB_ASYN;
    else
        acb -> acb_flags &= ~ACB_ASYN;

    if (SSetIndications (acb -> acb_fd, e (ssDATAser), e (ssTOKENser),
		e (ssSYNCser), e (ssACTIVITYser), e (ssREPORTser),
		e (ssFINISHser), e (ssABORTser), &sis) == NOTOK) {
        acb -> acb_flags &= ~ACB_ASYN;
	switch (sa -> sa_reason) {
	    case SC_WAITING: 
		return rosaplose (roi, ROS_WAITING, NULLCP, NULLCP);

	    default: 
		(void) ss2roslose (acb, roi, "SSetIndications", sa);
		freeacblk (acb);
		return NOTOK;
	}
    }


    return OK;
}

#undef	e

/*    map association descriptors for select() */

/* ARGSUSED */

int	ro2ssmask (acb, mask, nfds, roi)
register struct assocblk   *acb;
fd_set *mask;
int    *nfds;
struct RoSAPindication *roi;
{
    struct SSAPindication   sis;
    register struct SSAPabort  *sa = &sis.si_abort;

    if (SSelectMask (acb -> acb_fd, mask, nfds, &sis) == NOTOK)
	switch (sa -> sa_reason) {
	    case SC_WAITING: 
		return rosaplose (roi, ROS_WAITING, NULLCP, NULLCP);

	    default: 
		(void) ss2roslose (acb, roi, "SSelectMask", sa);
		freeacblk (acb);
		return NOTOK;
	}

    return OK;
}

/*    protocol-level abort */

int	ro2sslose (acb, result)
register struct assocblk   *acb;
int	result;
{
    int     len;
    char   *base;
    PE	    pe;
    struct SSAPindication   sis;

    base = NULL, len = 0;
/* begin AbortInformation PSDU (pseudo) */
    if (pe = pe_alloc (PE_CLASS_UNIV, PE_FORM_CONS, PE_CONS_SET)) {
	if (set_add (pe, num2prim ((integer) result, PE_CLASS_CONT, 0))
	        != NOTOK)
	    (void) pe2ssdu (pe, &base, &len);

	PLOGP (rosap_log,OACS_AbortInformation, pe, "AbortInformation",
	      0);

	pe_free (pe);
    }
/* end AbortInformation PSDU */

    (void) SUAbortRequest (acb -> acb_fd, base, len, &sis);
    acb -> acb_fd = NOTOK;

    if (base)
	free (base);
}

/*    SSAP interface */

int	ro2sswait (acb, invokeID, secs, roi)
register struct assocblk *acb;
int    *invokeID,
	secs;
register struct RoSAPindication *roi;
{
    int     result;
    struct SSAPdata sxs;
    register struct SSAPdata   *sx = &sxs;
    struct SSAPindication   sis;
    register struct SSAPindication *si = &sis;

    if (acb -> acb_apdu) {
	result = acb2osdu (acb, NULLIP, acb -> acb_apdu, roi);
	acb -> acb_apdu = NULLPE;

	return result;
    }

    for (;;) {
	switch (result = SReadRequest (acb -> acb_fd, sx, secs, si)) {
	    case NOTOK: 
		return doSSabort (acb, &si -> si_abort, roi);

	    case OK: 
		if ((result = doSSdata (acb, invokeID, sx, roi)) != OK)
		    return (result != DONE ? result : OK);
		continue;

	    case DONE: 
		switch (si -> si_type) {
		    case SI_TOKEN: 
			if (doSStokens (acb, &si -> si_token, roi) == NOTOK)
			    return NOTOK;
			continue;

		    case SI_SYNC: 
			if (doSSsync (acb, &si -> si_sync, roi) == NOTOK)
			    return NOTOK;
			continue;

		    case SI_ACTIVITY: 
			if (doSSactivity (acb, &si -> si_activity, roi) == NOTOK)
			    return NOTOK;
			continue;

		    case SI_REPORT: 
			if (doSSreport (acb, &si -> si_report, roi) == NOTOK)
			    return NOTOK;
			continue;

		    case SI_FINISH: 
			if (doSSfinish (acb, &si -> si_finish, roi) == NOTOK)
			    return NOTOK;
			return DONE;

		    default: 
			(void) ropktlose (acb, roi, ROS_PROTOCOL, NULLCP,
				"unknown indication (0x%x) from session",
				si -> si_type);
			break;
		}
		break;

	    default: 
		(void) ropktlose (acb, roi, ROS_PROTOCOL, NULLCP,
			"unexpected return from SReadRequest=%d", result);
		break;
	}
	break;
    }

    freeacblk (acb);

    return NOTOK;
}

/*  */

/* ARGSUSED */

int	ro2ssready (acb, priority, roi)
register struct assocblk *acb;
int	priority;
struct RoSAPindication *roi;
{
    int     result;
    PE	    pe;
    struct SSAPdata sxs;
    register struct SSAPdata *sx = &sxs;
    struct  SSAPindication sis;
    struct  SSAPindication *si = &sis;
    struct  SSAPabort *sa = &si -> si_abort;
	
    if (acb -> acb_apdu || (acb -> acb_flags & ACB_CLOSING))
	return rosaplose (roi, ROS_WAITING, NULLCP, NULLCP);

    if (!(acb -> acb_requirements & SR_HALFDUPLEX)
	    || (acb -> acb_flags & ACB_TURN))
	return OK;

    if (!(acb -> acb_flags & ACB_PLEASE)) {
	if (SPTokenRequest (acb -> acb_fd, ST_DAT_TOKEN, NULLCP, 0, si)
		== NOTOK) {
	    (void) ss2roslose (acb, roi, "SPTokenRequest", sa);
	    goto out;
	}

	acb -> acb_flags |= ACB_PLEASE;
    }

    for (;;) {
	switch (result = SReadRequest (acb -> acb_fd, sx, NOTOK, si)) {
	    case NOTOK: 
		return doSSabort (acb, &si -> si_abort, roi);

	    case OK: 
		if (sx -> sx_type != SX_NORMAL) {
		    (void) ropktlose (acb, roi, ROS_PROTOCOL, NULLCP,
			    "unexpected data indication (0x%x)",
			    sx -> sx_type);
		    goto bad_sx;
		}
		if (pe = qbuf2pe (&sx -> sx_qbuf, sx -> sx_cc, &result)) {
		    acb -> acb_apdu = pe;
		    return rosaplose (roi, ROS_WAITING, NULLCP, NULLCP);
		}
		if (result != PS_ERR_NMEM) {
		    (void) rosapreject (acb, roi, ROS_GP_STRUCT, NULLCP, "%s",
			    ps_error (result));
		    continue;
		}

		(void) ropktlose (acb, roi, ROS_PROTOCOL, NULLCP, "%s",
			ps_error (result));
bad_sx: ;
		SXFREE (sx);
		goto out;

	    case DONE: 
		switch (si -> si_type) {
		    case SI_TOKEN: 
			if (doSStokens (acb, &si -> si_token, roi) == NOTOK)
			    return NOTOK;
			return OK;

		    case SI_SYNC: 
			if (doSSsync (acb, &si -> si_sync, roi) == NOTOK)
			    return NOTOK;
			continue;

		    case SI_ACTIVITY: 
			if (doSSactivity (acb, &si -> si_activity, roi) == NOTOK)
			    return NOTOK;
			continue;

		    case SI_REPORT: 
			if (doSSreport (acb, &si -> si_report, roi) == NOTOK)
			    return NOTOK;
			continue;

		    case SI_FINISH: 
			if (doSSfinish (acb, &si -> si_finish, roi) == NOTOK)
			    return NOTOK;
			return DONE;

		    default: 
			(void) ropktlose (acb, roi, ROS_PROTOCOL, NULLCP,
				"unknown indication (0x%x) from session",
				si -> si_type);
			break;
		}
		break;

	    default: 
		(void) ropktlose (acb, roi, ROS_PROTOCOL, NULLCP,
			"unexpected return from SReadRequest=%d", result);
		break;
	}
	break;
    }

out: ;
    freeacblk (acb);

    return NOTOK;
}   
   
/*  */

/* ARGSUSED */

int	ro2sswrite (acb, pe, fe, priority, roi)
register struct assocblk *acb;
register PE pe;
PE	fe;
int	priority;
struct RoSAPindication *roi;
{
    int	    result;
    register struct udvec *vv;
    struct udvec *uv;
    struct SSAPindication   sis;
    register struct SSAPindication *si = &sis;
    register struct SSAPabort  *sa = &si -> si_abort;

    PLOGP (rosap_log,ROS_OPDU, pe, "OPDU", 0);

    uv = NULL;
    if ((result = pe2uvec (pe, &uv)) == NOTOK)
	(void) rosaplose (roi, ROS_CONGEST, NULLCP, "out of memory");
    else
	if ((result = SWriteRequest (acb -> acb_fd, 0, uv, si)) == NOTOK)
	    (void) ss2roslose (acb, roi, "SWriteRequest", sa);
	else
	    result = OK;

    if (fe)
	(void) pe_extract (pe, fe);
    pe_free (pe);

    if (uv) {
	for (vv = uv; vv -> uv_base; vv++)
	    if (!vv -> uv_inline)
		free ((char *) vv -> uv_base);
	free ((char *) uv);
    }

    if (result == NOTOK)
	freeacblk (acb);

    return result;
}

/*  */

static int  doSSdata (acb, invokeID, sx, roi)
register struct assocblk   *acb;
int    *invokeID;
register struct SSAPdata *sx;
struct RoSAPindication *roi;
{
    int     result;
    register PE	    pe;

    if (sx -> sx_type != SX_NORMAL) {
	(void) ropktlose (acb, roi, ROS_PROTOCOL, NULLCP,
		"unexpected data indication (0x%x)", sx -> sx_type);
	goto out;
    }

    if (pe = (*acb -> acb_getosdu) (&sx -> sx_qbuf, sx -> sx_cc, &result))
	return acb2osdu (acb, invokeID, pe, roi);

    if (result != PS_ERR_NMEM)
	return rosapreject (acb, roi, ROS_GP_STRUCT, NULLCP, "%s",
		ps_error (result));

    (void) ropktlose (acb, roi, ROS_CONGEST, NULLCP, "%s",
	    ps_error (result));

out: ;
    SXFREE (sx);

    freeacblk (acb);
    return NOTOK;
}

/*  */

static int  doSStokens (acb, st, roi)
register struct assocblk   *acb;
register struct SSAPtoken *st;
struct RoSAPindication *roi;
{
    int     result;
    struct SSAPindication   sis;
    struct SSAPindication  *si = &sis;
    struct SSAPabort   *sa = &si -> si_abort;

    switch (st -> st_type) {
	case ST_CONTROL: 
	    break;

	case ST_PLEASE: 
	    if (!(acb -> acb_requirements & SR_HALFDUPLEX))
		break;
	    if (!(acb -> acb_flags & ACB_TURN))
		break;		/* error - do not have turn */

	    result = SGTokenRequest (acb -> acb_fd, ST_DAT_TOKEN, si);

	    if (result == NOTOK) {
		(void) ss2roslose (acb, roi, "SGTokenRequest", sa);
		goto out;
	    }
	    acb -> acb_flags &= ~ACB_TURN;
	    STFREE (st);
	    return result;

	case ST_GIVE: 
	    if (!(acb -> acb_requirements & SR_HALFDUPLEX))
		break;
	    if (acb -> acb_flags & ACB_TURN)
		break;		/* error - have turn already */

	    if (st -> st_tokens & ST_DAT_TOKEN) {
		acb -> acb_flags |= ACB_TURN;
		acb -> acb_flags &= ~ACB_PLEASE;
	    }
	    return result;

	default: 
	    break;
    }

    (void) ropktlose (acb, roi, ROS_PROTOCOL, NULLCP,
	    "unexpected token indication (0x%x)", st -> st_type);

out: ;
    STFREE (st);

    freeacblk (acb);
    return NOTOK;
}

/*  */

static int  doSSsync (acb, sn, roi)
register struct assocblk   *acb;
register struct SSAPsync *sn;
struct RoSAPindication *roi;
{
    (void) ropktlose (acb, roi, ROS_PROTOCOL, NULLCP,
	    "unexpected sync indication (0x%x)", sn -> sn_type);

    SNFREE (sn);

    freeacblk (acb);
    return NOTOK;
}

/*  */

static int  doSSactivity (acb, sv, roi)
register struct assocblk   *acb;
register struct SSAPactivity *sv;
struct RoSAPindication *roi;
{
    (void) ropktlose (acb, roi, ROS_PROTOCOL, NULLCP,
	    "unexpected activity indication (0x%x)", sv -> sv_type);

    SVFREE (sv);

    freeacblk (acb);
    return NOTOK;
}

/*  */

static int  doSSreport (acb, sp, roi)
register struct assocblk   *acb;
register struct SSAPreport *sp;
struct RoSAPindication *roi;
{
    (void) ropktlose (acb, roi, ROS_PROTOCOL, NULLCP,
	    "unexpected exception report indication (0x%x)", sp -> sp_peer);

    SPFREE (sp);

    freeacblk (acb);
    return NOTOK;
}

/*  */

static int  doSSfinish (acb, sf, roi)
register struct assocblk   *acb;
struct SSAPfinish *sf;
struct RoSAPindication *roi;
{
    SFFREE (sf);

    if (acb -> acb_flags & ACB_INIT) {
	(void) ropktlose (acb, roi, ROS_PROTOCOL, NULLCP,
		"association management botched");
	freeacblk (acb);
	return NOTOK;
    }

    acb -> acb_flags |= ACB_FINN;
    roi -> roi_type = ROI_END;
    {
	register struct RoSAPend   *roe = &roi -> roi_end;

	bzero ((char *) roe, sizeof *roe);
    }

    return DONE;
}

/*  */

int	ss2rosabort (acb, sa, roi)
register struct assocblk   *acb;
register struct SSAPabort *sa;
struct RoSAPindication *roi;
{
    int	    result;
    register PE	pe;
    struct type_OACS_AbortInformation *pabort
			    = (struct type_OACS_AbortInformation *) 0;
    int acsap_abort = -1;

    if (!sa -> sa_peer) {
	if (sa -> sa_reason == SC_TIMER)
	    return rosaplose (roi, ROS_TIMER, NULLCP, NULLCP);

	(void) ss2roslose (acb, roi, NULLCP, sa);
	goto out;
    }

    if (sa -> sa_cc == 0) {
	(void) rosaplose (roi, ROS_ABORTED, NULLCP, NULLCP);
	goto out;
    }

    if ((pe = ssdu2pe (sa -> sa_info, sa -> sa_cc, NULLCP, &result))
	    == NULLPE) {
	(void) rosaplose (roi, ROS_PROTOCOL, NULLCP, NULLCP);
	goto out;
    }
    result = parse_OACS_AbortInformation (pe, 1, NULLIP, NULLVP, NULLCP);

#ifdef	DEBUG
    if (result != NOTOK && (rosap_log -> ll_events & LLOG_PDUS))
	pvpdu (rosap_log, print_OACS_AbortInformation_P, pe,
	       "AbortInformation", 1);
#endif

    pe_free (pe);
    if (result == NOTOK) {
	(void) rosaplose (roi, ROS_PROTOCOL, "%s", PY_pepy);
	goto out;
    }
    acsap_abort = pabort->member_OACS_6->parm;
    switch (acsap_abort) {
	case ABORT_LSP: 
	case ABORT_TMP: 
	    result = ROS_REMOTE;
	    break;

	default: 
	    result = ROS_PROTOCOL;
	    break;
    }
    (void) rosaplose (roi, result, NULLCP, NULLCP);

out: ;
    SAFREE (sa);
    acb -> acb_fd = NOTOK;
    freeacblk (acb);
    if (pabort)
	free_OACS_AbortInformation(pabort);

    return NOTOK;
}

/*  */

static int  ssDATAser (sd, sx)
int	sd;
register struct SSAPdata *sx;
{
    IFP	    handler;
    register struct assocblk   *acb;
    struct RoSAPindication  rois;
    register struct RoSAPindication *roi = &rois;

    if ((acb = findacblk (sd)) == NULL)
	return;
    handler = acb -> acb_rosindication;

    if (doSSdata (acb, NULLIP, sx, roi) != OK)
	(*handler) (sd, roi);
}

/*  */

static int  ssTOKENser (sd, st)
int	sd;
register struct SSAPtoken *st;
{
    IFP	    handler;
    register struct assocblk   *acb;
    struct RoSAPindication  rois;
    register struct RoSAPindication *roi = &rois;

    if ((acb = findacblk (sd)) == NULL)
	return;
    handler = acb -> acb_rosindication;

    if (doSStokens (acb, st, roi) != OK)
	(*handler) (sd, roi);
}

/*  */

static int  ssSYNCser (sd, sn)
int	sd;
register struct SSAPsync *sn;
{
    IFP	    handler;
    register struct assocblk   *acb;
    struct RoSAPindication  rois;
    register struct RoSAPindication *roi = &rois;

    if ((acb = findacblk (sd)) == NULL)
	return;
    handler = acb -> acb_rosindication;

    if (doSSsync (acb, sn, roi) != OK)
	(*handler) (sd, roi);
}

/*  */

static int  ssACTIVITYser (sd, sv)
int	sd;
register struct SSAPactivity *sv;
{
    IFP	    handler;
    register struct assocblk   *acb;
    struct RoSAPindication  rois;
    register struct RoSAPindication *roi = &rois;

    if ((acb = findacblk (sd)) == NULL)
	return;
    handler = acb -> acb_rosindication;

    if (doSSactivity (acb, sv, roi) != OK)
	(*handler) (sd, roi);
}

/*  */

static int  ssREPORTser (sd, sp)
int	sd;
register struct SSAPreport *sp;
{
    IFP	    handler;
    register struct assocblk   *acb;
    struct RoSAPindication  rois;
    register struct RoSAPindication *roi = &rois;

    if ((acb = findacblk (sd)) == NULL)
	return;
    handler = acb -> acb_rosindication;

    if (doSSreport (acb, sp, roi) != OK)
	(*handler) (sd, roi);
}

/*  */

static int  ssFINISHser (sd, sf)
int	sd;
struct SSAPfinish *sf;
{
    IFP	    handler;
    register struct assocblk   *acb;
    struct RoSAPindication  rois;
    register struct RoSAPindication *roi = &rois;

    if ((acb = findacblk (sd)) == NULL)
	return;
    handler = acb -> acb_rosindication;

    (void) doSSfinish (acb, sf, roi);

    (*handler) (sd, roi);
}

/*  */

static int  ssABORTser (sd, sa)
int	sd;
register struct SSAPabort *sa;
{
    IFP	    handler;
    register struct assocblk   *acb;
    struct RoSAPindication  rois;
    register struct RoSAPindication *roi = &rois;

    if ((acb = findacblk (sd)) == NULL)
	return;
    handler = acb -> acb_rosindication;

    (void) doSSabort (acb, sa, roi);

    (*handler) (sd, roi);
}

/*  */

int	ss2roslose (acb, roi, event, sa)
register struct assocblk *acb;
register struct RoSAPindication *roi;
char   *event;
register struct SSAPabort *sa;
{
    int     reason;
    char   *cp,
            buffer[BUFSIZ];

    if (event)
	SLOG (rosap_log, LLOG_EXCEPTIONS, NULLCP,
	      (sa -> sa_cc > 0 ? "%s: %s [%*.*s]": "%s: %s", event,
	       SErrString (sa -> sa_reason), sa -> sa_cc, sa -> sa_cc,
	       sa -> sa_data));

    cp = "";
    switch (sa -> sa_reason) {
	case SC_SSAPID: 
	case SC_SSUSER: 
	case SC_ADDRESS: 
	    reason = ROS_ADDRESS;
	    break;

	case SC_REFUSED:
	    reason = ROS_REFUSED;
	    break;

	case SC_CONGEST: 
	    reason = ROS_CONGEST;
	    break;

	default: 
	    (void) sprintf (cp = buffer, " (%s at session)",
		    SErrString (sa -> sa_reason));
	case SC_TRANSPORT:
	case SC_ABORT:
	    reason = ROS_SESSION;
	    break;
    }

    if (sa -> sa_cc > 0)
	return ropktlose (acb, roi, reason, NULLCP, "%*.*s%s",
		sa -> sa_cc, sa -> sa_cc, sa -> sa_data, cp);
    else
	return ropktlose (acb, roi, reason, NULLCP, "%s", *cp ? cp + 1 : cp);
}

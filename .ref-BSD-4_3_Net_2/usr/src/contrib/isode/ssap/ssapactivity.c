/* ssapactitivity.c - SPM: activities */

#ifndef	lint
static char *rcsid = "$Header: /f/osi/ssap/RCS/ssapactivity.c,v 7.1 91/02/22 09:45:42 mrose Interim $";
#endif

/* 
 * $Header: /f/osi/ssap/RCS/ssapactivity.c,v 7.1 91/02/22 09:45:42 mrose Interim $
 *
 *
 * $Log:	ssapactivity.c,v $
 * Revision 7.1  91/02/22  09:45:42  mrose
 * Interim 6.8
 * 
 * Revision 7.0  89/11/23  22:25:22  mrose
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
#include "spkt.h"

/*    S-CONTROL-GIVE.REQUEST */

int	SGControlRequest (sd, si)
int	sd;
struct SSAPindication *si;
{
    SBV	    smask;
    int     result;
    register struct ssapblk *sb;

    smask = sigioblock ();

    ssapPsig (sb, sd);

    result = SGControlRequestAux (sb, si);

    (void) sigiomask (smask);

    return result;
}

/*  */

static int  SGControlRequestAux (sb, si)
register struct ssapblk *sb;
register struct SSAPindication *si;
{
    int     result;
    register struct ssapkt *s;

    if (SDoActivityAux (sb, si, 1, 1) == NOTOK)
	return NOTOK;

    if (sb -> sb_flags & SB_GTC)
	return ssaplose (si, SC_OPERATION, NULLCP, "give control in progress");

    if ((s = newspkt (SPDU_GTC)) == NULL)
	return ssaplose (si, SC_CONGEST, NULLCP, "out of memory");

    if ((result = spkt2sd (s, sb -> sb_fd, 0, si)) == NOTOK)
	freesblk (sb);
    else {
	sb -> sb_owned = 0;
	sb -> sb_flags |= SB_GTC;
    }
    freespkt (s);

    return result;
}

/*    S-ACTIVITY-START.REQUEST */

int	SActStartRequest (sd, id, data, cc, si)
int	sd;
struct SSAPactid *id;
char   *data;
int	cc;
struct SSAPindication *si;
{
    SBV	    smask;
    int     result;
    register struct ssapblk *sb;

    missingP (id);
    idmuchP (id);
    missingP (si);

    smask = sigioblock ();

    ssapPsig (sb, sd);
    toomuchP (sb, data, cc, SV_SIZE, "activity start");

    result = SActStartRequestAux (sb, id, data, cc, si);

    (void) sigiomask (smask);

    return result;
}

/*  */

static int  SActStartRequestAux (sb, id, data, cc, si)
register struct ssapblk *sb;
struct SSAPactid *id;
char   *data;
int	cc;
register struct SSAPindication *si;
{
    int result;

    if (SDoActivityAux (sb, si, 1, 0) == NOTOK)
	return NOTOK;

    if ((result = SWriteRequestAux (sb, SPDU_AS, data, cc, 0, 0L, 0, id,
		NULLSD, NULLSR, si)) == NOTOK)
	freesblk (sb);
    else {
	sb -> sb_V_A = sb -> sb_V_M = sb -> sb_V_R = 1;
	sb -> sb_flags |= SB_Vact;
    }

    return result;
}

/*    S-ACTIVITY-RESUME.REQUEST */

int	SActResumeRequest (sd, id, oid, ssn, ref, data, cc, si)
int	sd;
struct SSAPactid *id,
		 *oid;
long	ssn;
struct SSAPref *ref;
char   *data;
int	cc;
struct SSAPindication *si;
{
    SBV	    smask;
    int     result;
    register struct ssapblk *sb;

    missingP (id);
    idmuchP (id);
    missingP (oid);
    idmuchP (oid);
    if (SERIAL_MIN > ssn || ssn > SERIAL_MAX)
	return ssaplose (si, SC_PARAMETER, NULLCP, "invalid serial number");
#ifdef	notdef
    missingP (ref);
#endif
    if (ref)
	refmuchP (ref)
    missingP (si);

    smask = sigioblock ();

    ssapPsig (sb, sd);
    toomuchP (sb, data, cc, SV_SIZE, "activity resume");

    result = SActResumeRequestAux (sb, id, oid, ssn, ref, data, cc, si);

    (void) sigiomask (smask);

    return result;
}

/*  */

static int  SActResumeRequestAux (sb, id, oid, ssn, ref, data, cc, si)
register struct ssapblk *sb;
struct SSAPactid *id,
		 *oid;
long	ssn;
struct SSAPref *ref;
char   *data;
int	cc;
register struct SSAPindication *si;
{
    int	    result;

    if (SDoActivityAux (sb, si, 1, 0) == NOTOK)
	return NOTOK;

    if ((result = SWriteRequestAux (sb, SPDU_AR, data, cc, 0, ssn, 0, id,
		oid, ref, si)) == NOTOK)
	freesblk (sb);
    else {
	sb -> sb_V_A = sb -> sb_V_M = ssn + 1;
	sb -> sb_V_R = 1;
	sb -> sb_flags |= SB_Vact;
    }

    return result;
}

/*    S-ACTIVITY-INTERRUPT.REQUEST */

int	SActIntrRequest (sd, reason, si)
int	sd;
int	reason;
struct SSAPindication *si;
{
    SBV	    smask;
    int     result;
    register struct ssapblk *sb;

    if (!(SP_OK (reason)))
	return ssaplose (si, SC_PARAMETER, NULLCP, "invalid reason");
    missingP (si);

    smask = sigioblock ();

    ssapXsig (sb, sd);
    if (sb -> sb_flags & SB_MAP) {
	(void) sigsetmask (smask);
	return ssaplose (si, SC_OPERATION, NULLCP, "majorsync in progress");
    }

    result = SActIntrRequestAux (sb, reason, SPDU_AI, si);

    (void) sigiomask (smask);

    return result;
}

/*  */

static int  SActIntrRequestAux (sb, reason, type, si)
register struct ssapblk *sb;
int	reason,
	type;
register struct SSAPindication *si;
{
    int	    result;

    if (!(sb -> sb_requirements & SR_ACTIVITY))
	return ssaplose (si, SC_OPERATION, NULLCP,
		"activity management service unavailable");
    if (!(sb -> sb_owned & ST_ACT_TOKEN))
	return ssaplose (si, SC_OPERATION, NULLCP,
		"activity token not owned by you");
    if (!(sb -> sb_flags & SB_Vact))
	return ssaplose (si, SC_OPERATION, NULLCP,
		"no activity in progress");
    if ((sb -> sb_flags & SB_RA)
	    && SDoCollideAux (sb -> sb_flags & SB_INIT ? 1 : 0,
			type == SPDU_AI ? SYNC_INTR : SYNC_DISC, 0L,
			sb -> sb_rs, sb -> sb_rsn) == NOTOK)
	return ssaplose (si, SC_OPERATION, NULLCP,
		"resync in progress takes precedence");

    if ((result = SWriteRequestAux (sb, type, NULLCP, 0, reason, 0L, 0,
	    NULLSD, NULLSD, NULLSR, si)) == NOTOK)
	freesblk (sb);
    else {
	sb -> sb_flags |= SB_AI, sb -> sb_flags &= ~(SB_RA | SB_EDACK | SB_ERACK);
	sb -> sb_rs = type == SPDU_AI ? SYNC_INTR : SYNC_DISC;
    }

    return result;
}

/*    S-ACTIVITY-INTERRUPT.RESPONSE */

int	SActIntrResponse (sd, si)
int	sd;
struct SSAPindication *si;
{
    SBV	    smask;
    int     result;
    register struct ssapblk *sb;

    missingP (si);

    smask = sigioblock ();

    ssapAsig (sb, sd);

    result = SActIntrResponseAux (sb, SPDU_AIA, si);

    (void) sigiomask (smask);

    return result;
}

/*  */

static int  SActIntrResponseAux (sb, type, si)
register struct ssapblk *sb;
int	type;
register struct SSAPindication *si;
{
    int	    result;

    if (!(sb -> sb_requirements & SR_ACTIVITY))
	return ssaplose (si, SC_OPERATION, NULLCP,
		"activity management service unavailable");
    if (!(sb -> sb_flags & SB_Vact))
	return ssaplose (si, SC_OPERATION, NULLCP,
		"no activity in progress");
    if (!(sb -> sb_flags & SB_AIA))
	return ssaplose (si, SC_OPERATION, NULLCP,
		"no activity interrupt/discard in progress");

    if ((result = SWriteRequestAux (sb, type, NULLCP, 0, 0, 0L, 0,
	    NULLSD, NULLSD, NULLSR, si)) == NOTOK)
	freesblk (sb);
    else {
	sb -> sb_flags &= ~(SB_AIA | SB_Vact);
	sb -> sb_owned = 0;
    }

    return result;
}

/*    S-ACTIVITY-DISCARD.REQUEST */

int	SActDiscRequest (sd, reason, si)
int	sd;
int	reason;
struct SSAPindication *si;
{
    SBV	    smask;
    int     result;
    register struct ssapblk *sb;

    if (!(SP_OK (reason)))
	return ssaplose (si, SC_PARAMETER, NULLCP, "invalid reason");
    missingP (si);

    smask = sigioblock ();

    ssapXsig (sb, sd);
    if (sb -> sb_flags & SB_MAP) {
	(void) sigsetmask (smask);
	return ssaplose (si, SC_OPERATION, NULLCP, "majorsync in progress");
    }

    result = SActIntrRequestAux (sb, reason, SPDU_AD, si);

    (void) sigiomask (smask);

    return result;
}

/*    S-ACTIVITY-DISCARD.RESPONSE */

int	SActDiscResponse (sd, si)
int	sd;
struct SSAPindication *si;
{
    SBV	    smask;
    int     result;
    register struct ssapblk *sb;

    missingP (si);

    smask = sigioblock ();

    ssapAsig (sb, sd);

    result = SActIntrResponseAux (sb, SPDU_ADA, si);

    (void) sigiomask (smask);

    return result;
}

/*    S-ACTIVITY-END.REQUEST */

int	SActEndRequest (sd, ssn, data, cc, si)
int	sd;
long   *ssn;
char   *data;
int	cc;
struct SSAPindication *si;
{
    SBV	    smask;
    int     result;
    register struct ssapblk *sb;

    missingP (ssn);
    missingP (si);

    smask = sigioblock ();

    ssapPsig (sb, sd);
    toomuchP (sb, data, cc, SV_SIZE, "activity end");

    result = SMajSyncRequestAux (sb, ssn, data, cc, 0, si);

    (void) sigiomask (smask);

    return result;
}

/*    S-ACTIVITY-END.RESPONSE */

int	SActEndResponse (sd, data, cc, si)
int	sd;
char   *data;
int	cc;
struct SSAPindication *si;
{
    SBV	    smask;
    int     result;
    register struct ssapblk *sb;

    missingP (si);

    smask = sigioblock ();

    ssapPsig (sb, sd);
    toomuchP (sb, data, cc, SV_SIZE, "activity end");

    result = SMajSyncResponseAux (sb, data, cc, si);

    (void) sigiomask (smask);

    return result;
}

/* rt2ssinitiat.c - RTPM: initiator */

#ifndef	lint
static char *rcsid = "$Header: /f/osi/rtsap/RCS/rt2ssinitiat.c,v 7.4 91/02/22 09:42:30 mrose Interim $";
#endif

/* 
 * $Header: /f/osi/rtsap/RCS/rt2ssinitiat.c,v 7.4 91/02/22 09:42:30 mrose Interim $
 *
 *
 * $Log:	rt2ssinitiat.c,v $
 * Revision 7.4  91/02/22  09:42:30  mrose
 * Interim 6.8
 * 
 * Revision 7.3  90/10/23  20:44:14  mrose
 * update
 * 
 * Revision 7.2  90/07/27  08:47:57  mrose
 * update
 * 
 * Revision 7.1  90/07/01  21:07:10  mrose
 * pepsy
 * 
 * Revision 6.1  89/06/24  00:55:57  mrose
 * reason
 * 
 * Revision 6.0  89/03/18  23:43:20  mrose
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
#include "OACS-types.h"
#include "rtpkt.h"
#include "isoservent.h"
#include "tailor.h"

/*    RT-BEGIN.REQUEST (X.410 OPEN.REQUEST) */

int	RtBeginRequest2 (called, calling, mode, turn, data, rtc, rti)
struct RtSAPaddr *called, *calling;
int	mode,
	turn;
PE	data;
struct RtSAPconnect *rtc;
struct RtSAPindication *rti;
{
    SBV     smask;
    int     result;

    isodetailor (NULLCP, 0);

    missingP (called);
    switch (mode) {
	case RTS_MONOLOGUE:
	case RTS_TWA:
	    break;

	default:
	    return rtsaplose (rti, RTS_PARAMETER, NULLCP,
		    "bad value for mode parameter");
    }
    switch (turn) {
	case RTS_INITIATOR:
	case RTS_RESPONDER:
	    break;

	default:
	    return rtsaplose (rti, RTS_PARAMETER, NULLCP,
		    "bad value for turn parameter");
    }
    missingP (rtc);
    bzero ((char *) rtc, sizeof *rtc);
    missingP (rti);

    smask = sigioblock ();

    result = RtBeginRequestAux (called, calling, mode, turn, data, rtc, rti);

    (void) sigiomask (smask);

    return result;
}

/*  */

static int  RtBeginRequestAux (called, calling, mode, turn, data, rtc, rti)
struct RtSAPaddr *called, *calling;
int	mode,
	turn;
PE	data;
struct RtSAPconnect *rtc;
struct RtSAPindication *rti;
{
    int	    len,
	    result,
	    settings;
    char   *base;
#ifdef	notdef
    register struct isoservent *is;
#endif
    register PE	pe,
		p,
		q,
		r;
    register struct assocblk *acb;
    struct SSAPref srs;
    register struct SSAPref *sr = &srs;
    struct SSAPconnect scs;
    register struct SSAPconnect *sc = &scs;
    struct SSAPindication sis;
    register struct SSAPindication *si = &sis;
    register struct SSAPabort *sa = &si -> si_abort;
    struct type_OACS_PAccept *paccpt = (struct type_OACS_PAccept *)0;

    if ((acb = newacblk ()) == NULL)
	return rtsaplose (rti, RTS_CONGEST, NULLCP, "out of memory");

/* begin PConnect PSDU */
    if ((pe = pe_alloc (PE_CLASS_UNIV, PE_FORM_CONS, PE_CONS_SET)) == NULLPE) {
no_mem: ;
	result = rtsaplose (rti, RTS_CONGEST, NULLCP, "out of memory");
	goto out1;
    }
    if (set_add (pe, p = pe_alloc (PE_CLASS_CONT, PE_FORM_CONS, PCONN_DTS))
		== NOTOK
	    || set_add (p, num2prim ((integer) SYN_X409, PE_CLASS_CONT,
				     DTS_SYNTAX)) == NOTOK
	    || set_add (pe, p = pe_alloc (PE_CLASS_CONT, PE_FORM_CONS,
		    PCONN_DATA)) == NOTOK
	    || (DEFAULT_CKPOINT != PCONN_CK_DFLT
		    && set_add (p, num2prim ((integer) DEFAULT_CKPOINT,
					     PE_CLASS_CONT,
					     PCONN_DATA_CK)) == NOTOK)
	    || (DEFAULT_WINDOW != PCONN_WD_DFLT
		    && set_add (p, num2prim ((integer) DEFAULT_WINDOW,
					     PE_CLASS_CONT,
					     PCONN_DATA_WD)) == NOTOK)
	    || set_add (p, num2prim ((integer) (mode == RTS_TWA ? PCONN_DM_TWA
		    : PCONN_DM_MONO), PE_CLASS_CONT, PCONN_DATA_DM)) == NOTOK
	    || set_add (p, q = pe_alloc (PE_CLASS_CONT, PE_FORM_CONS,
		    PCONN_DATA_CN)) == NOTOK
	    || set_add (q, r = pe_alloc (PE_CLASS_CONT, PE_FORM_CONS,
		    CN_OPEN)) == NOTOK
	    || set_add (r, data ? data : pe_alloc (PE_CLASS_CONT,
		    PE_FORM_PRIM, 0)) == NOTOK
	    || set_add (p, num2prim ((integer) ntohs (called -> rta_port),
		    PE_CLASS_CONT, PCONN_DATA_AP)) == NOTOK)
	goto no_mem;
/* end PConnect PSDU */

    PLOGP (rtsap_log,OACS_PConnect, pe, "PConnect", 0);

    if (pe2ssdu (pe, &base, &len) == NOTOK)
	goto no_mem;

    if (data)
	(void) pe_extract (pe, data), data = NULLPE;
    pe_free (pe);
    pe = NULLPE;

#ifdef	notdef		/* SEK doesn't like this */
    if (called -> rta_addr.sa_selectlen == 0) {
	if ((is = getisoserventbyname ("rts", "ssap")) == NULL) {
	    result = rtsaplose (rti, RTS_ADDRESS, NULLCP,
			    "ssap/rts: unknown entity");
	    goto out2;
	}
	if (is -> is_selectlen > SSSIZE) {	/* XXX */
	    result = rosaplose (rti, RTS_ADDRESS, NULLCP,
			"ssap/rts: selector too long (%d octets)",
			is -> is_selectlen);
	    goto out2;
	}
	bcopy (is -> is_selector, called -> rta_addr.sa_selector,
	    called -> rta_addr.sa_selectlen = is -> is_selectlen);
    }
#endif

    acb -> acb_requirements = RTS_MYREQUIRE;
    settings = 0;
#define dotoken(requires,shift,bit,type) \
{ \
    if (acb -> acb_requirements & requires) \
	if (turn == RTS_INITIATOR) \
	    settings |= ST_INIT_VALUE << shift; \
	else \
	    settings |= ST_RESP_VALUE << shift; \
}
    dotokens ();
#undef	dotoken

    if ((sr = addr2ref (SLocalHostName ())) == NULL) {
	result = rtsaplose (rti, RTS_CONGEST, NULLCP, "out of memory");
	goto out2;
    }
    acb -> acb_connect = *sr;	/* struct copy */

    if (SConnRequest (sr, calling ? &calling -> rta_addr : NULLSA,
		      &called -> rta_addr, acb -> acb_requirements,
	    settings, SERIAL_NONE, base, len, NULLQOS, sc, si) == NOTOK) {
	result = ss2rtslose (NULLACB, rti, "SConnRequest", sa);
	goto out2;
    }
    free (base);

    if (sc -> sc_result == SC_ACCEPT) {
	acb -> acb_fd = sc -> sc_sd;
	acb -> acb_uabort = SUAbortRequest;
    }
    else
        if (sc -> sc_result == SC_ABORT) {
	    acb -> acb_fd = NOTOK;

	    (void) ss2rtsabort (acb, sa, rti);

	    rtc -> rtc_sd = NOTOK;
	    rtc -> rtc_result = RTS_ABORTED;

	    return OK;
	}

    if ((pe = ssdu2pe (sc -> sc_data, sc -> sc_cc, NULLCP, &result))
	    == NULLPE) {
	if (sc -> sc_result != SC_ACCEPT) {
	    bzero ((char *) sa, sizeof *sa);
	    sa -> sa_reason = sc -> sc_result;
	    acb -> acb_fd = NOTOK;
	    (void) ss2rtslose (acb, rti, "SConnRequest(pseudo)", sa);

	    rtc -> rtc_sd = NOTOK;
	    rtc -> rtc_result = rti -> rti_abort.rta_reason;

	    result = OK;
	}
	else
	    result = rtpktlose (acb, rti, result != PS_ERR_NMEM ? RTS_PROTOCOL
		    : RTS_CONGEST, NULLCP, "%s", ps_error (result));
	goto out1;
    }

    SCFREE (sc);

    if (sc -> sc_result != SC_ACCEPT) {
	struct type_OACS_PRefuse *pref;

	if (parse_OACS_PRefuse (pe, 1, NULLIP, NULLVP, &pref) == NOTOK) {
	    result = pylose ();
	    goto out1;
	}

	PLOGP (rtsap_log,OACS_PRefuse, pe, "PRefuse", 1);

	pe_free (pe);
	
	freeacblk (acb);

	rtc -> rtc_sd = NOTOK;
	switch (pref->parm) {
	    case REFUSE_BUSY: 
		rtc -> rtc_result = RTS_BUSY;
		break;

	    case REFUSE_VALIDATE: 
		rtc -> rtc_result = RTS_VALIDATE;
		break;

	    case REFUSE_MODE:
		rtc -> rtc_result = RTS_MODE;
		break;

	    default: 
		rtc -> rtc_result = RTS_PROTOCOL;
		break;
	}

	free_OACS_RefuseReason(pref);
	return OK;
    }

    acb -> acb_flags = ACB_CONN | ACB_RTS | ACB_INIT;
    SetSS2RtService (acb);
    if (turn == RTS_INITIATOR)
	acb -> acb_flags |= ACB_TURN;
    if (mode == RTS_TWA)
	acb -> acb_flags |= ACB_TWA;
    if ((acb -> acb_requirements = sc -> sc_requirements) != RTS_MYREQUIRE) {
	result = rtpktlose (acb, rti, RTS_PROTOCOL, NULLCP,
		    "desired session requirements denied");
	goto out1;
    }

#define dotoken(requires,shift,bit,type) \
{ \
    if (acb -> acb_requirements & requires) \
	switch (sc -> sc_settings & (ST_MASK << shift)) { \
	    case ST_INIT_VALUE << shift: \
		acb -> acb_owned |= bit; \
		acb -> acb_avail |= bit; \
		break; \
 \
	    case ST_RESP_VALUE << shift: \
		acb -> acb_avail |= bit; \
		break; \
 \
	    default: \
		result = rtpktlose (acb, rti, RTS_PROTOCOL, NULLCP, \
			    "%s token management botched", type); \
		goto out1; \
	} \
}
	dotokens ();
#undef	dotoken
    switch (turn) {
	case RTS_INITIATOR:
	    if (acb -> acb_owned == acb -> acb_avail)
		break;
	    result = rtsaplose (rti, RTS_PROTOCOL, NULLCP,
		    "token management botched");
	    goto out1;

	case RTS_RESPONDER:
	    if (acb -> acb_owned == 0)
		break;
	    result = rtsaplose (rti, RTS_PROTOCOL, NULLCP,
		    "token management botched");
	    goto out1;
    }
    acb -> acb_ssdusize = sc -> sc_ssdusize;

    PLOGP (rtsap_log,OACS_PAccept, pe, "PAccept", 1);

    if (parse_OACS_PAccept (pe, 1, NULLIP, NULLVP, &paccpt) == NOTOK) {
	result = pylose ();
	goto out1;
    }

    acb -> acb_ckpoint = paccpt -> pUserData -> checkpointSize;
    acb -> acb_window = paccpt -> pUserData -> windowsize;

    rtc -> rtc_sd = acb -> acb_fd;
    rtc -> rtc_result = RTS_ACCEPT;
    {
	struct type_OACS_ConnectionData *pdat = paccpt -> pUserData -> member_OACS_5;

	if (pdat -> offset == type_OACS_ConnectionData_open)
	    rtc -> rtc_data = pe_expunge (pe, pdat -> un.open);
	else
	    rtc -> rtc_data = NULLPE;
    }

    if (paccpt)
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
    if (paccpt)
	free_OACS_PAccept (paccpt);

    return result;
}

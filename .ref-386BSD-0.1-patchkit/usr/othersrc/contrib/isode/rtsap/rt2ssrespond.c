/* rt2ssrespond.c - RTPM: responder */

#ifndef	lint
static char *rcsid = "$Header: /f/osi/rtsap/RCS/rt2ssrespond.c,v 7.5 91/02/22 09:42:33 mrose Interim $";
#endif

/* 
 * $Header: /f/osi/rtsap/RCS/rt2ssrespond.c,v 7.5 91/02/22 09:42:33 mrose Interim $
 *
 *
 * $Log:	rt2ssrespond.c,v $
 * Revision 7.5  91/02/22  09:42:33  mrose
 * Interim 6.8
 * 
 * Revision 7.4  90/11/05  13:33:02  mrose
 * update
 * 
 * Revision 7.3  90/10/23  20:44:16  mrose
 * update
 * 
 * Revision 7.2  90/07/27  08:48:01  mrose
 * update
 * 
 * Revision 7.1  90/07/01  21:07:16  mrose
 * pepsy
 * 
 * Revision 6.0  89/03/18  23:43:23  mrose
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
#include "RTS-types.h"
#include "OACS-types.h"
#include "rtpkt.h"
#include "tailor.h"

/*    DATA */

extern int acsap_conntype;

/*    RT-BEGIN.INDICATION (X.410 OPEN.INDICATION) */

int	RtBInit (vecp, vec, rts, rti)
int	vecp;
char  **vec;
struct RtSAPstart *rts;
struct RtSAPindication *rti;
{
    int     len,
	    result;
    char   *base;
    register struct assocblk   *acb;
    register PE	pe;
    struct SSAPref ref;
    struct SSAPstart    sss;
    register struct SSAPstart  *ss = &sss;
    struct SSAPindication   sis;
    register struct SSAPindication *si = &sis;
    register struct SSAPabort  *sa = &si -> si_abort;
    struct type_OACS_PConnect *pcon;
    int sc_reason = SC_CONGESTION;

    isodetailor (NULLCP, 0);

    missingP (vec);
    missingP (rts);
    missingP (rti);

    if ((acb = newacblk ()) == NULL)
	return rtsaplose (rti, RTS_CONGEST, NULLCP, "out of memory");
    acb -> acb_flags |= ACB_RTS;
    SetSS2RtService (acb);

    if (SInit (vecp, vec, ss, si) == NOTOK) {
	(void) ss2rtslose (acb, rti, "SInit", sa);
	goto out1;
    }

    acb -> acb_fd = ss -> ss_sd;
    acb -> acb_uabort = SUAbortRequest;

    base = NULLCP, len = 0;
    acb -> acb_connect = ss -> ss_connect;	/* struct copy */
    if ((ss -> ss_requirements &= RTS_MYREQUIRE) != RTS_MYREQUIRE) {
	(void) rtsaplose (rti, RTS_PROTOCOL, NULLCP,
		    "desired session requirements unavailable");
	goto out2;
    }
    acb -> acb_requirements = ss -> ss_requirements;

#define dotoken(requires,shift,bit,type) \
{ \
    if (acb -> acb_requirements & requires) \
	switch (ss -> ss_settings & (ST_MASK << shift)) { \
	    case ST_INIT_VALUE << shift: \
		acb -> acb_avail |= bit; \
		break; \
 \
	    case ST_RESP_VALUE << shift: \
		acb -> acb_owned |= bit; \
		acb -> acb_avail |= bit; \
		break; \
 \
	    default: \
		(void) rtsaplose (rti, RTS_PROTOCOL, NULLCP, \
			"%s token management botched", type); \
		goto out2; \
	} \
    }
    dotokens ();
#undef	dotoken
    if (acb -> acb_owned != 0 && acb -> acb_owned != acb -> acb_avail) {
	(void) rtsaplose (rti, RTS_PROTOCOL, NULLCP,
		"token management botched");
	goto out2;
    }
    if (acb -> acb_owned)
	acb -> acb_flags |= ACB_TURN;
    acb -> acb_settings = ss -> ss_settings;
    acb -> acb_ssdusize = ss -> ss_ssdusize;

    if ((pe = ssdu2pe (ss -> ss_data, ss -> ss_cc, NULLCP, &result))
	    == NULLPE) {
	(void) rtsaplose (rti, result != PS_ERR_NMEM ? RTS_PROTOCOL
		: RTS_CONGEST, NULLCP, "%s", ps_error (result));
	goto out2;
    }

    SSFREE (ss);

    if (parse_OACS_PConnect (pe, 1, NULLIP, NULLVP, &pcon) == NOTOK) {
	(void) pylose ();
	pe_free (pe);
	goto out2;
    }

    PLOGP (rtsap_log,OACS_PConnect, pe, "PConnect", 1);

    if (pcon -> pUserData -> member_OACS_2->offset
	    == type_OACS_ConnectionData_recover) {
	(void) rtsaplose (rti, RTS_CONGEST, NULLCP,
			  "rejecting attempted recovery");
	pe_free (pe);
	if ((pe = pe_alloc (PE_CLASS_UNIV, PE_FORM_CONS, PE_CONS_SET))
	        && set_add (pe, num2prim ((integer) RTS_RECOVER, PE_CLASS_CONT,
					  PREF_REASON)) != NOTOK) {
	    PLOGP (rtsap_log,OACS_PRefuse, pe, "PRefuse", 0);
	    if (pe2ssdu (pe, &base, &len) == NOTOK) {
		if (base)
		    free (base), base = NULL;
		len = 0;
	    }
	}
	if (pe)
	    pe_free (pe);
	free_OACS_PConnect(pcon);
	sc_reason = SC_REJECTED;
	goto out2;
    }

    acb -> acb_ckpoint = acb -> acb_ssdusize >> 10;
    if ((0 < pcon -> pUserData -> checkpointSize
       && pcon -> pUserData -> checkpointSize < acb -> acb_ckpoint)
	    || acb -> acb_ckpoint <= 0)
	acb -> acb_ckpoint = pcon -> pUserData -> checkpointSize;
    acb -> acb_window = pcon -> pUserData -> windowSize;

    bzero ((char *) rts, sizeof *rts);
    rts -> rts_sd = acb -> acb_fd;
    rts -> rts_initiator.rta_addr = ss -> ss_calling;	/* struct copy */
    if (pcon->pUserData->dialogueMode == PCONN_DM_TWA)
	rts -> rts_mode = RTS_TWA, acb -> acb_flags |= ACB_TWA;
    else
	rts -> rts_mode = RTS_MONOLOGUE;
    rts -> rts_turn = acb -> acb_flags & ACB_TURN ? RTS_RESPONDER
	    : RTS_INITIATOR;
    rts -> rts_port = htons ((u_short) pcon -> pUserData -> applicationProtocol);
    if (pcon -> pUserData -> member_OACS_2 -> offset
	    == type_OACS_ConnectionData_open)
	rts -> rts_data = pe_expunge (pe, pcon -> pUserData -> member_OACS_2
							    -> un.open);
    else
	rts -> rts_data = NULLPE;
    
    free_OACS_PConnect(pcon);
    return OK;

out2: ;
    bzero ((char *) &ref, sizeof ref);
    (void) SConnResponse (acb -> acb_fd, &ref, NULLSA, sc_reason, 0, 0,
	    SERIAL_NONE, base, len, si);
    acb -> acb_fd = NOTOK;
    if (base)
	free (base);

out1: ;
    SSFREE (ss);
    freeacblk (acb);

    return NOTOK;
}

/*    RT-BEGIN.RESPONSE (X.410 OPEN.RESPONSE) */

int	RtBeginResponse (sd, status, data, rti)
int	sd;
int	status;
PE	data;
struct RtSAPindication *rti;
{
    int	    len,
	    result;
    char   *base;
    register PE	pe,
		p,
		q,
		r;
    register struct assocblk   *acb;
    struct SSAPref ref;
    struct SSAPindication sis;
    register struct SSAPindication *si = &sis;
    register struct SSAPabort *sa = &si -> si_abort;

    if ((acb = findacblk (sd)) == NULL || (acb -> acb_flags & ACB_CONN))
	return rtsaplose (rti, RTS_PARAMETER, NULLCP,
		"invalid association descriptor");
    if ((acb -> acb_flags & ACB_ACS) || !(acb -> acb_flags & ACB_RTS))
	return rtsaplose (rti, RTS_OPERATION, NULLCP,
		"not an association descriptor for RTS");
    switch (status) {
	case RTS_ACCEPT: 
	    break;

	case RTS_MODE:
	case RTS_VALIDATE: 
	case RTS_BUSY: 
	    if (data)
		return rtsaplose (rti, RTS_PARAMETER, NULLCP,
			"user data not permitted when refusing association");
	    break;

	default: 
	    return rtsaplose (rti, RTS_PARAMETER, NULLCP,
		    "bad value for status parameter");
    }
    missingP (rti);

    base = NULLCP;
    switch (status) {
	case RTS_ACCEPT: 
/* begin PAccept PSDU */
	    if ((pe = pe_alloc (PE_CLASS_UNIV, PE_FORM_CONS, PE_CONS_SET))
		    == NULLPE) {
	no_mem: ;
		(void) rtsaplose (rti, RTS_CONGEST, NULLCP, "out of memory");
		goto out1;
	    }
	    if (set_add (pe, p = pe_alloc (PE_CLASS_CONT, PE_FORM_CONS,
			    PACC_DTS)) == NOTOK
		    || set_add (p, num2prim ((integer) SYN_X409, PE_CLASS_CONT,
			    DTS_SYNTAX)) == NOTOK
		    || set_add (pe, p = pe_alloc (PE_CLASS_CONT, PE_FORM_CONS,
			    PACC_DATA)) == NOTOK
		    || set_add (p, num2prim ((integer) acb -> acb_ckpoint,
			    PE_CLASS_CONT, PACC_DATA_CK)) == NOTOK
		    || set_add (p, num2prim ((integer) acb -> acb_window,
			    PE_CLASS_CONT, PACC_DATA_WD)) == NOTOK
		    || set_add (p, q = pe_alloc (PE_CLASS_CONT, PE_FORM_CONS,
			    PACC_DATA_CN)) == NOTOK
		    || set_add (q, r = pe_alloc (PE_CLASS_CONT, PE_FORM_CONS,
			    CN_OPEN)) == NOTOK
		    || set_add (r, data ? data : pe_alloc (PE_CLASS_UNIV,
						PE_FORM_PRIM, PE_PRIM_NULL))
			    == NOTOK)
		goto no_mem;
/* end PAccept PSDU */
	    status = SC_ACCEPT;
	    break;

	default: 
/* begin PRefuse PSDU */
	    if ((pe = pe_alloc (PE_CLASS_UNIV, PE_FORM_CONS, PE_CONS_SET))
		    == NULLPE
		    || set_add (pe, num2prim ((integer) status, PE_CLASS_CONT,
			    PREF_REASON)) == NOTOK)
		goto no_mem;
/* end PRefuse PSDU */
	    status = SC_REJECTED;
	    break;
    }

#ifdef	DEBUG
    if (rtsap_log -> ll_events & LLOG_PDUS)
	if (status == SC_ACCEPT)
	    pvpdu (rtsap_log, print_OACS_PAccept_P, pe, "PAccept", 0);
	else
	    pvpdu (rtsap_log, print_OACS_PRefuse_P, pe, "PRefuse", 0);
#endif

    if (pe2ssdu (pe, &base, &len) == NOTOK)
	goto no_mem;

    if (SConnResponse (acb -> acb_fd, &acb -> acb_connect, NULLSA, status,
		acb -> acb_requirements, acb -> acb_settings, SERIAL_NONE,
		base, len, si) == NOTOK) {
	acb -> acb_fd = NOTOK;
	(void) ss2rtslose (acb, rti, "SConnResponse", sa);
	goto out3;
    }

    if (status == SC_ACCEPT)
	acb -> acb_flags |= ACB_CONN;
    else {
	acb -> acb_fd = NOTOK;
	freeacblk (acb);
    }
    result = OK;

out2: ;
    if (pe) {
	if (data)
	    (void) pe_extract (pe, data);
	pe_free (pe);
    }
    if (base)
	free (base);

    return result;

out1: ;
    bzero ((char *) &ref, sizeof ref);
    (void) SConnResponse (acb -> acb_fd, &ref, NULLSA, SC_CONGEST, 0, 0,
	    SERIAL_NONE, NULLCP, 0, si);
    acb -> acb_fd = NOTOK;
out3: ;
    freeacblk (acb);
    result = NOTOK;
    goto out2;
}

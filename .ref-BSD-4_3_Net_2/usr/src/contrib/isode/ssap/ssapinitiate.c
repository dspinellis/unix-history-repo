/* ssapinitiate.c - SPM: initiator */

#ifndef	lint
static char *rcsid = "$Header: /f/osi/ssap/RCS/ssapinitiate.c,v 7.3 91/02/22 09:45:48 mrose Interim $";
#endif

/* 
 * $Header: /f/osi/ssap/RCS/ssapinitiate.c,v 7.3 91/02/22 09:45:48 mrose Interim $
 *
 *
 * $Log:	ssapinitiate.c,v $
 * Revision 7.3  91/02/22  09:45:48  mrose
 * Interim 6.8
 * 
 * Revision 7.2  90/01/27  10:27:27  mrose
 * touch-up
 * 
 * Revision 7.1  89/11/27  10:30:40  mrose
 * sync
 * 
 * Revision 7.0  89/11/23  22:25:27  mrose
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
#include "tailor.h"

/*    S-(ASYN-)CONNECT.REQUEST */

#define	dotoken(requires,shift,bit,type) \
{ \
    if (requirements & requires) \
	switch (settings & (ST_MASK << shift)) { \
	    case ST_INIT_VALUE << shift: \
	    case ST_RESP_VALUE << shift: \
	    case ST_CALL_VALUE << shift: \
		break; \
 \
	    default: \
		return ssaplose (si, SC_PARAMETER, NULLCP, \
			"improper choice of %s token setting", type); \
	} \
}

/*  */

int	SAsynConnRequest (ref, calling, called, requirements, settings, isn,
	data, cc, qos, sc, si, async)
struct SSAPref *ref;
struct SSAPaddr *calling,
		*called;
int	requirements,
	settings,
	cc,
	async;
long	isn;
char   *data;
struct QOStype *qos;
struct SSAPconnect *sc;
struct SSAPindication *si;
{
    SBV     smask;
    int     result;

    isodetailor (NULLCP, 0);

    missingP (ref);
    refmuchP (ref);
    if (ref -> sr_vlen)
	return ssaplose (si, SC_PARAMETER, NULLCP, "bad format for reference");
#ifdef	notdef
    missingP (calling);
#endif
    missingP (called);

    if (requirements & ~SR_MYREQUIRE)
	return ssaplose (si, SC_PARAMETER, NULLCP,
		"requirements settings not supported");
    if ((requirements & SR_EXCEPTIONS)
	    && !(requirements & SR_HALFDUPLEX))
	return ssaplose (si, SC_PARAMETER, NULLCP,
		"exception service requires half-duplex service");

    dotokens ();

    if (requirements & (SR_MINORSYNC | SR_MAJORSYNC | SR_RESYNC)) {
	if (!(requirements & SR_ACTIVITY) || isn != SERIAL_NONE)
	    if (SERIAL_MIN > isn || isn > SERIAL_MAX + 1)
		return ssaplose (si, SC_PARAMETER, NULLCP,
			"bad choice for initial serial number");
    }
    else
	if (isn != SERIAL_NONE)
	    return ssaplose (si, SC_PARAMETER, NULLCP,
		    "initial serial number invalid given requirements");

    if (data == NULL)
	cc = 0;
    else
	if (cc > CONNECT_MAX)
	    return ssaplose (si, SC_PARAMETER, NULLCP,
			     "too much initial user data, %d octets", cc);

#ifdef	notdef
    missingP (qos);
#endif
    missingP (sc);
    missingP (si);

    smask = sigioblock ();

    result = SConnRequestAux (ref, calling, called, requirements, settings,
	    isn, data, cc, qos, sc, si, async);

    (void) sigiomask (smask);

    return result;
}

#undef	dotoken

/*  */

static int  SConnRequestAux (ref, calling, called, requirements, settings, isn,
	    data, cc, qos, sc, si, async)
struct SSAPref *ref;
struct SSAPaddr *calling,
		*called;
int	requirements,
	settings,
	cc,
	async;
long	isn;
char   *data;
struct QOStype *qos;
struct SSAPconnect *sc;
struct SSAPindication *si;
{
    int     result;
    register struct ssapkt *s;
    register struct ssapblk *sb;
    struct TSAPconnect  tcs;
    register struct TSAPconnect *tc = &tcs;
    struct TSAPdisconnect   tds;
    register struct TSAPdisconnect *td = &tds;

    if ((sb = newsblk ()) == NULL)
	return ssaplose (si, SC_CONGEST, NULLCP, "out of memory");
    if (!async || qos == NULLQOS || qos -> qos_maxtime <= 0)
	sb -> sb_maxtime = NOTOK;
    else
	sb -> sb_maxtime = qos -> qos_maxtime;

    if ((s = newspkt (SPDU_CN)) == NULL) {
	(void) ssaplose (si, SC_CONGEST, NULLCP, "out of memory");
	goto out1;
    }

#ifdef	notdef
    if (called -> sa_selectlen > 0) {
	if (calling == NULLSA) {
	    static struct SSAPaddr sas;

	    calling = &sas;
	    bzero ((char *) calling, sizeof *calling);
	}

	if (calling -> sa_selectlen == 0) {
	    calling -> sa_port =
			    htons ((u_short) (0x8000 | (getpid () & 0x7fff)));
	    calling -> sa_selectlen = sizeof calling -> sa_port;
	}
    }
#endif

    if (calling) {
	if (calling -> sa_selectlen > 0) {
	    s -> s_mask |= SMASK_CN_CALLING;
	    bcopy (calling -> sa_selector, s -> s_calling,
		s -> s_callinglen = calling -> sa_selectlen);
	}
	sb -> sb_initiating = *calling;	/* struct copy */
    }

    if (called -> sa_selectlen > 0) {
	s -> s_mask |= SMASK_CN_CALLED;
	bcopy (called -> sa_selector, s -> s_called,
		s -> s_calledlen = called -> sa_selectlen);
    }
    sb -> sb_responding = *called;	/* struct copy */

    sb -> sb_requirements = requirements;
    sb -> sb_settings = settings;

    if ((result = TAsynConnRequest (calling ? &calling -> sa_addr : NULLTA,
		&called -> sa_addr,
		(qos ? qos -> qos_extended : 0)
			|| ((requirements & SR_EXPEDITED) ? 1 : 0),
		NULLCP, 0, qos, tc, td, async)) == NOTOK) {
	(void) ts2sslose (si, "TAsynConnRequest", td);

	bzero ((char *) sc, sizeof *sc);
	sc -> sc_sd = NOTOK;
	sc -> sc_result = si -> si_abort.sa_reason;

	result = DONE;
	goto out2;
    }

    sb -> sb_fd = tc -> tc_sd;
    if (qos && qos -> qos_sversion < 0) {
	sb -> sb_version = SB_VRSN1;
	sb -> sb_vrsnmask = SB_ALLVRSNS;
    }
    else
	sb -> sb_vrsnmask = 1 << (sb -> sb_version =
				    (cc > SS_SIZE
					   || (qos && qos -> qos_sversion > 1))
				       ? SB_VRSN2 : SB_VRSN1);

    s -> s_mask |= SMASK_CN_REF | SMASK_CN_OPT | SMASK_CN_VRSN;
    s -> s_cn_reference = *ref;	/* struct copy */
    s -> s_options = CR_OPT_NULL;
    s -> s_cn_version = sb -> sb_vrsnmask;

    if (isn != SERIAL_NONE) {
	s -> s_mask |= SMASK_CN_ISN;
	s -> s_isn = isn;
    }

    if (cc > 0) {		/* XXX: user musn't touch! */
	s -> s_mask |= SMASK_UDATA_PGI;
	s -> s_udata = data, s -> s_ulen = cc;
    }
    else
	s -> s_udata = NULL, s -> s_ulen = 0;

    sb -> sb_retry = s;
    if (async) {
	switch (result) {
	case CONNECTING_1:
	case CONNECTING_2:
	    sc -> sc_sd = sb -> sb_fd;
	    return result;
	}
    }
    if ((result = SAsynRetryAux (sb, tc, sc, si)) == DONE && !async)
	result = OK;
    return result;

out2: ;
    freespkt (s);
out1: ;
    freesblk (sb);

    return result;
}

/*    S-ASYN-RETRY.REQUEST (pseudo) */

int	SAsynRetryRequest (sd, sc, si)
int	sd;
struct SSAPconnect *sc;
struct SSAPindication *si;
{
    SBV     smask;
    int     result;
    register struct ssapblk *sb;
    struct TSAPconnect  tcs;
    register struct TSAPconnect *tc = &tcs;
    struct TSAPdisconnect   tds;
    register struct TSAPdisconnect *td = &tds;

    missingP (sc);
    missingP (si);

    smask = sigioblock ();

    if ((sb = findsblk (sd)) == NULL) {
	(void) sigiomask (smask);
	return ssaplose (si, SC_PARAMETER, NULLCP,
		"invalid session descriptor");
    }
    if (sb -> sb_flags & SB_CONN) {
	(void) sigiomask (smask);
	return ssaplose (si, SC_OPERATION, NULLCP,
		"session descriptor connected");
    }

    switch (result = TAsynRetryRequest (sb -> sb_fd, tc, td)) {
	case NOTOK: 
	    (void) ts2sslose (si, "TAsynRetryRequest", td);
	    sb -> sb_fd = NOTOK;

	    bzero ((char *) sc, sizeof *sc);
	    sc -> sc_sd = NOTOK;
	    sc -> sc_result = si -> si_abort.sa_reason;

	    result = DONE;
	    freesblk (sb);
	    break;

	case CONNECTING_1:
	case CONNECTING_2:
	    break;

	case DONE: 
	    result = SAsynRetryAux (sb, tc, sc, si);
	    break;
    }

    (void) sigiomask (smask);

    return result;
}

/*    S-ASYN-NEXT.REQUEST (pseudo) */

int	SAsynNextRequest (sd, sc, si)
int	sd;
struct SSAPconnect *sc;
struct SSAPindication *si;
{
    SBV     smask;
    int     result;
    register struct ssapblk *sb;
    struct TSAPconnect  tcs;
    register struct TSAPconnect *tc = &tcs;
    struct TSAPdisconnect   tds;
    register struct TSAPdisconnect *td = &tds;

    missingP (sc);
    missingP (si);

    smask = sigioblock ();

    if ((sb = findsblk (sd)) == NULL) {
	(void) sigiomask (smask);
	return ssaplose (si, SC_PARAMETER, NULLCP,
		"invalid session descriptor");
    }
    if (sb -> sb_flags & SB_CONN) {
	(void) sigiomask (smask);
	return ssaplose (si, SC_OPERATION, NULLCP,
		"session descriptor connected");
    }

    switch (result = TAsynNextRequest (sb -> sb_fd, tc, td)) {
	case NOTOK: 
	    (void) ts2sslose (si, "TAsynRetryRequest", td);
	    sb -> sb_fd = NOTOK;

	    bzero ((char *) sc, sizeof *sc);
	    sc -> sc_sd = NOTOK;
	    sc -> sc_result = si -> si_abort.sa_reason;

	    result = DONE;
	    freesblk (sb);
	    break;

	case CONNECTING_1:
	case CONNECTING_2:
	    break;

	case DONE: 
	    result = SAsynRetryAux (sb, tc, sc, si);
	    break;
    }

    (void) sigiomask (smask);

    return result;
}

/*  */

#define	dotoken(requires,shift,bit,type) \
{ \
    if (sb -> sb_requirements & requires) { \
	switch (sb -> sb_settings & (ST_MASK << shift)) { \
	    case ST_CALL_VALUE << shift: \
		if (!(s -> s_mask & SMASK_CN_SET)) \
		    s -> s_settings = ST_INIT_VALUE << shift; \
		switch (s -> s_settings & (ST_MASK << shift)) { \
		    case ST_INIT_VALUE << shift: \
		    default: \
			sb -> sb_owned |= bit; \
			sc -> sc_settings |= ST_INIT_VALUE << shift; \
			break; \
 \
		    case ST_RESP_VALUE << shift: \
			sc -> sc_settings |= ST_RESP_VALUE << shift; \
			break; \
		} \
		break; \
 \
	    case ST_INIT_VALUE << shift: \
		sb -> sb_owned |= bit; \
		sc -> sc_settings |= ST_INIT_VALUE << shift; \
		break; \
 \
	    case ST_RESP_VALUE << shift: \
		sc -> sc_settings |= ST_RESP_VALUE << shift; \
		break; \
	} \
 \
	if ((s -> s_mask & SMASK_AC_TOKEN) && (s -> s_ac_token & bit)) \
	    sc -> sc_please |= bit; \
    } \
}

/*  */

static int  SAsynRetryAux (sb, tc, sc, si)
register struct ssapblk *sb;
struct TSAPconnect *tc;
struct SSAPconnect *sc;
struct SSAPindication *si;
{
    int	    len,
	    result;
    register struct ssapkt *s;

    s = sb -> sb_retry;
    sb -> sb_retry = NULL;

    sb -> sb_responding.sa_addr = tc -> tc_responding;	/* struct copy */
    if (tc -> tc_expedited)
	sb -> sb_flags |= SB_EXPD;
    else
	sb -> sb_requirements &= ~SR_EXPEDITED;
    if (sb -> sb_version < SB_VRSN2)		/* XXX */
	sb -> sb_tsdu_us = sb -> sb_tsdu_them =
					    GET_TSDU_SIZE (tc -> tc_tsdusize);

    if (sb -> sb_tsdu_us || sb -> sb_tsdu_them) {
	s -> s_mask |= SMASK_CN_TSDU;
	s -> s_tsdu_resp = GET_TSDU_SIZE (sb -> sb_tsdu_us);
	s -> s_tsdu_init = GET_TSDU_SIZE (sb -> sb_tsdu_them);
    }

    s -> s_mask |= SMASK_CN_REQ;
    if ((s -> s_cn_require = sb -> sb_requirements) & SR_TOKENS) {
	s -> s_mask |= SMASK_CN_SET;
	s -> s_settings = sb -> sb_settings;
    }

    result = spkt2sd (s, sb -> sb_fd, 0, si);
    s -> s_mask &= ~SMASK_UDATA_PGI;
    s -> s_udata = NULL, s -> s_ulen = 0;

    freespkt (s);
    if (result == NOTOK)
	goto out1;

    if ((s = sb2spkt (sb, si, sb -> sb_maxtime, NULLTX)) == NULL) {
	if (si -> si_abort.sa_reason == SC_TIMER)
	    (void) ssaplose (si, SC_CONGEST, NULLCP, "remote SPM timed-out");
	result = NOTOK;
	goto out2;
    }

    bzero ((char *) sc, sizeof *sc);
    switch (s -> s_code) {
	case SPDU_AC: 
	    sc -> sc_sd = sb -> sb_fd;
	    sc -> sc_result = SC_ACCEPT;
	    if (s -> s_mask & SMASK_CN_REF)
		sc -> sc_connect = s -> s_cn_reference;	/* struct copy */
	    if (s -> s_mask & SMASK_CN_OPT)
		sb -> sb_options = s -> s_options;
	    if (!(s -> s_mask & SMASK_CN_TSDU))
		s -> s_tsdu_init = s -> s_tsdu_resp = 0;
	    if (s -> s_tsdu_init < sb -> sb_tsdu_us)
		sb -> sb_tsdu_us = s -> s_tsdu_init;
	    if (s -> s_tsdu_resp < sb -> sb_tsdu_them)
		sb -> sb_tsdu_them = s -> s_tsdu_resp;
	    if (BAD_TSDU_SIZE (sb -> sb_tsdu_us)) {
		result = spktlose (sb -> sb_fd, si, SC_PROTOCOL, NULLCP,
			"perposterous TSDU size (%d) for initiator",
			sb -> sb_tsdu_us);
		goto out2;
	    }
	    if (BAD_TSDU_SIZE (sb -> sb_tsdu_them)) {
		result = spktlose (sb -> sb_fd, si, SC_PROTOCOL, NULLCP,
			"perposterous TSDU size (%d) for responder",
			sb -> sb_tsdu_them);
		goto out2;
	    }
	    if (s -> s_mask & SMASK_CN_VRSN) {
		if (!(s -> s_cn_version & sb -> sb_vrsnmask)) {
							/* not SC_VERSION */
		    result = spktlose (sb -> sb_fd, si, SC_PROTOCOL, NULLCP,
		     "version mismatch: expecting something in 0x%x, got 0x%x",
				       sb -> sb_vrsnmask, s -> s_cn_version);
		    goto out2;
		}
		sb -> sb_vrsnmask &= s -> s_cn_version;
	    }
	    sb -> sb_version = (sb -> sb_vrsnmask & (1 << SB_VRSN2))
					? SB_VRSN2 : SB_VRSN1;
	    if (s -> s_mask & SMASK_CN_ISN)
		sc -> sc_isn = sb -> sb_V_A = sb -> sb_V_M = s -> s_isn;
	    else
		sc -> sc_isn = SERIAL_NONE;
	    if (!(s -> s_mask & SMASK_CN_REQ)) {
		s -> s_mask |= SMASK_CN_REQ;
		s -> s_cn_require = SR_DEFAULT;
	    }
	    switch (sb -> sb_requirements & (SR_HALFDUPLEX | SR_DUPLEX)) {
		case SR_HALFDUPLEX: 
		    if (s -> s_cn_require & SR_HALFDUPLEX)
			break;
		    result = spktlose (sb -> sb_fd, si, SC_PROTOCOL, NULLCP,
			    "half-duplex negotiation failed");
		    goto out2;

		case SR_DUPLEX: 
		    if (s -> s_cn_require & SR_DUPLEX)
			break;
		    result = spktlose (sb -> sb_fd, si, SC_PROTOCOL, NULLCP,
			    "full-duplex negotiation failed");
		    goto out2;

		default: 
		    break;
	    }
#ifdef	notdef		/* screwy session protocol... */
	    if (s -> s_cn_require & ~sb -> sb_requirements) {
		result = spktlose (sb -> sb_fd, si, SC_PROTOCOL, NULLCP,
			    "requirements negotiation failed");
		goto out2;
	    }
#endif
	    sb -> sb_requirements &= s -> s_cn_require;
	    switch (sb -> sb_requirements & (SR_HALFDUPLEX | SR_DUPLEX)) {
		case SR_HALFDUPLEX: 
		case SR_DUPLEX: 
		    break;

		default: 
		    result = spktlose (sb -> sb_fd, si, SC_PROTOCOL, NULLCP,
			    "half/full-duplex negotiation failed");
		    goto out2;
	    }
	    if ((sb -> sb_requirements & SR_EXCEPTIONS)
		    && !(sb -> sb_requirements & SR_HALFDUPLEX)) {
		result = spktlose (sb -> sb_fd, si, SC_PROTOCOL, NULLCP,
			"exception service requires half-duplex service");
		goto out2;
	    }
	    sc -> sc_requirements = sb -> sb_requirements;
	    sc -> sc_settings = sc -> sc_please = 0;
	    dotokens ();
	    if (s -> s_mask & SMASK_CN_CALLED) {
		if ((len = s -> s_calledlen)
			> sizeof sb -> sb_responding.sa_selector)
		    len = sizeof sb -> sb_responding.sa_selector;
		bcopy (s -> s_called, sb -> sb_responding.sa_selector,
			sb -> sb_responding.sa_selectlen = len);
	    }
	    sc -> sc_responding = sb -> sb_responding;	/* struct copy */
	    if ((sc -> sc_ssdusize = sb -> sb_tsdu_us - SSDU_MAGIC) < 0)
		sc -> sc_ssdusize = tc -> tc_tsdusize - SSDU_MAGIC;
	    sc -> sc_qos = tc -> tc_qos;	/* struct copy */
	    sc -> sc_qos.qos_sversion = sb -> sb_version + 1;
	    sc -> sc_qos.qos_extended = (sb -> sb_flags & SB_EXPD) ? 1 : 0;
	    copySPKTdata (s, sc);

	    freespkt (s);
	    sb -> sb_flags |= SB_CONN | SB_INIT;

	    return DONE;

	case SPDU_RF: 		/* ignore s -> s_rf_disconnect */
	    sc -> sc_sd = NOTOK;
	    sc -> sc_result = s -> s_rlen > 0 ? *s -> s_rdata
		: SC_NOTSPECIFIED;
	    if (s -> s_mask & SMASK_RF_REF)
		sc -> sc_connect = s -> s_rf_reference;	/* struct copy */
	    if ((sc -> sc_result == SC_REJECTED)
		    && (s -> s_mask & SMASK_RF_REQ))
		sc -> sc_requirements = s -> s_rf_require;
	    if ((s -> s_mask & SMASK_CN_CALLED)
		    && (sc -> sc_result & SC_BASE)) {
		if ((len = s -> s_calledlen)
			> sizeof sb -> sb_responding.sa_selector)
		    len = sizeof sb -> sb_responding.sa_selector;
		bcopy (s -> s_called, sb -> sb_responding.sa_selector,
			sb -> sb_responding.sa_selectlen = len);
	    }
	    sc -> sc_responding = sb -> sb_responding;	/* struct copy */
	    sc -> sc_data = s -> s_rdata + 1, sc -> sc_cc = s -> s_rlen - 1;
	    sc -> sc_realdata = s -> s_rdata, s -> s_rdata = NULL;
	    si -> si_type = SI_ABORT;
	    {
		register struct SSAPabort  *sa = &si -> si_abort;

		sa -> sa_peer = 0;
		sa -> sa_reason = sc -> sc_result;
		sa -> sa_info = sc -> sc_data, sa -> sa_cc = sc -> sc_cc;
		sa -> sa_realinfo = NULL;
	    }
	    result = DONE;
	    break;

	case SPDU_AB: 
	    sc -> sc_sd = NOTOK;
	    sc -> sc_result = SC_ABORT;
	    si -> si_type = SI_ABORT;
	    {
		register struct SSAPabort  *sa = &si -> si_abort;

		if (!(sa -> sa_peer = (s -> s_ab_disconnect & AB_DISC_USER)
			    ? 1 : 0))
		    sa -> sa_reason = sc -> sc_result;
		sa -> sa_info = s -> s_udata, sa -> sa_cc = s -> s_ulen;
		sa -> sa_realinfo = s -> s_udata, s -> s_udata = NULL;
	    }
	    result = DONE;
	    break;

	default: 
	    result = spktlose (sb -> sb_fd, si, SC_PROTOCOL, NULLCP,
		    "session protocol mangled: expecting 0x%x, got 0x%x",
		    SPDU_AC, s -> s_code);
	    break;
    }

out2: ;
    freespkt (s);
out1: ;
    freesblk (sb);

    return result;
}

#undef	dotoken

/* ssaprespond.c - SPM: responder */

#ifndef	lint
static char *rcsid = "$Header: /f/osi/ssap/RCS/ssaprespond.c,v 7.3 91/02/22 09:45:59 mrose Interim $";
#endif

/* 
 * $Header: /f/osi/ssap/RCS/ssaprespond.c,v 7.3 91/02/22 09:45:59 mrose Interim $
 *
 *
 * $Log:	ssaprespond.c,v $
 * Revision 7.3  91/02/22  09:45:59  mrose
 * Interim 6.8
 * 
 * Revision 7.2  90/11/21  11:31:45  mrose
 * sun
 * 
 * Revision 7.1  89/11/27  10:30:46  mrose
 * sync
 * 
 * Revision 7.0  89/11/23  22:25:38  mrose
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
#include "spkt.h"
#include "tailor.h"

/*    S-CONNECT.INDICATION */

int	SInit (vecp, vec, ss, si)
int	vecp;
char  **vec;
struct SSAPstart *ss;
struct SSAPindication *si;
{
    int	    len;
    register struct ssapblk *sb;
    register struct ssapkt *s;
    struct TSAPstart tss;
    register struct TSAPstart *ts = &tss;
    struct TSAPdisconnect tds;
    register struct TSAPdisconnect *td = &tds;

    isodetailor (NULLCP, 0);

    if (vecp < 2)
	return ssaplose (si, SC_PARAMETER, NULLCP,
		    "bad initialization vector");
    missingP (vec);
    missingP (ss);
    missingP (si);

    if ((sb = newsblk ()) == NULL)
	return ssaplose (si, SC_CONGEST, NULLCP, "out of memory");

    if (vecp == 2 || TInit (vecp, vec, ts, td) != NOTOK) {
	int	sd;
	struct TSAPdata txs;
	register struct TSAPdata *tx = &txs;

	if (vecp == 2) {
	    if (TRestoreState (vec[1], ts, td) == NOTOK) {
		(void) ts2sslose (si, "TRestoreState", td);
		(void) ssaplose (si, SC_PARAMETER, NULLCP,
				 "bad initialization vector");
		goto out1;
	    }
	    bzero (vec[0], strlen (vec[0]));
	    bzero (vec[1], strlen (vec[1]));
	    *vec = NULL;
	}
	else {
	    if (TConnResponse (ts -> ts_sd, &ts -> ts_called,
		    ts -> ts_expedited,  NULLCP, 0, NULLQOS, td) == NOTOK) {
	        (void) ts2sslose (si, "TConnResponse", td);
	        (void) TDiscRequest (ts -> ts_sd, NULLCP, 0, td);
	        goto out1;
	    }
	}
	sd = ts -> ts_sd;

	if (TReadRequest (sb -> sb_fd = sd, tx, NOTOK, td) == NOTOK) {
	    (void) ts2sslose (si, "TReadRequest", td);
	    goto out1;
	}

	s = tsdu2spkt (&tx -> tx_qbuf, tx -> tx_cc, NULLIP);
	TXFREE (tx);

	if (s == NULL || s -> s_errno != SC_ACCEPT) {
	    (void) spktlose (sd, si, (s ? s -> s_errno : SC_CONGEST)
				| SC_REFUSE, NULLCP, NULLCP);
	    goto out2;
	}

	if (s -> s_code != SPDU_CN) {
	    (void) spktlose (sd, si, (s ? s -> s_errno : SC_CONGEST)
				| SC_REFUSE, NULLCP,
			"session protocol mangled: expected 0x%x, got 0x%x",
			SPDU_CN, s -> s_code);
	    goto out2;
	}

	if (s -> s_mask & SMASK_CN_VRSN
	        && !(s -> s_cn_version & SB_ALLVRSNS)) {
	    (void) spktlose (sd, si, SC_VERSION | SC_REFUSE, NULLCP,
			     "version mismatch: expecting something in 0x%x, got 0x%x",
			     SB_ALLVRSNS, s -> s_cn_version);
	    goto out2;
	}
    }
    else {
	int	reason;

	vec += vecp - 2;
	s = NULL;
	if ((reason = td -> td_reason) != DR_PARAMETER
		|| TRestoreState (vec[0], ts, td) == NOTOK
		|| (s = str2spkt (vec[1])) == NULL
		|| s -> s_errno != SC_ACCEPT) {
	    if (s)
		freespkt (s);
	    else
		(void) ts2sslose (si, reason != DR_PARAMETER ? "TInit"
				: "TRestoreState", td);
	    (void) ssaplose (si, SC_PARAMETER, NULLCP,
			"bad initialization vector");
	    goto out1;
	}
	bzero (vec[0], strlen (vec[0]));
	bzero (vec[1], strlen (vec[1]));
	*vec = NULL;
    }

    sb -> sb_fd = ts -> ts_sd;
    sb -> sb_version =
		(s -> s_mask & SMASK_CN_VRSN)
		    ? ((s -> s_cn_version & (1 << SB_VRSN2))
			    ? SB_VRSN2 : SB_VRSN1)
		    : s -> s_ulen > SS_SIZE
			    ? SB_VRSN2 : SB_VRSN1;
    if (ts -> ts_expedited)
	sb -> sb_flags |= SB_EXPD;

    bzero ((char *) ss, sizeof *ss);
    ss -> ss_sd = sb -> sb_fd;
    if (s -> s_mask & SMASK_CN_REF)
	ss -> ss_connect = s -> s_cn_reference;	/* struct copy */
    if (s -> s_mask & SMASK_CN_OPT)
	sb -> sb_options = s -> s_options;
    if (s -> s_mask & SMASK_CN_ISN)
	ss -> ss_isn = sb -> sb_V_A = sb -> sb_V_M = s -> s_isn;
    else
	ss -> ss_isn = SERIAL_NONE;
    if (!(s -> s_mask & SMASK_CN_TSDU))
	s -> s_tsdu_init = s -> s_tsdu_resp = 0;
    if (s -> s_tsdu_init
		< (sb -> sb_tsdu_them = GET_TSDU_SIZE (ts -> ts_tsdusize)))
	sb -> sb_tsdu_them = s -> s_tsdu_init;
    if (s -> s_tsdu_resp
		< (sb -> sb_tsdu_us = GET_TSDU_SIZE (ts -> ts_tsdusize)))
	sb -> sb_tsdu_us = s -> s_tsdu_resp;
    if (sb -> sb_version >= SB_VRSN2)		/* XXX */
	sb -> sb_tsdu_them = sb -> sb_tsdu_us = 0;

    if (s -> s_mask & SMASK_CN_SET)
	sb -> sb_settings = ss -> ss_settings = s -> s_settings;
    sb -> sb_requirements = (s -> s_mask & SMASK_CN_REQ ? s -> s_cn_require
		: SR_DEFAULT) & SR_MYREQUIRE;
    if (!ts -> ts_expedited)
	sb -> sb_requirements &= ~SR_EXPEDITED;
    if (!(sb -> sb_requirements & SR_HALFDUPLEX))
	sb -> sb_requirements &= ~SR_EXCEPTIONS;
    ss -> ss_requirements = sb -> sb_requirements;
    ss -> ss_calling.sa_addr = ts -> ts_calling;	/* struct copy */
    if (s -> s_mask & SMASK_CN_CALLING) {
	if ((len = s -> s_callinglen)
		> sizeof ss -> ss_calling.sa_selector)
	    len = sizeof ss -> ss_calling.sa_selector;
	bcopy (s -> s_calling, ss -> ss_calling.sa_selector,
		ss -> ss_calling.sa_selectlen = len);
    }
    sb -> sb_initiating = ss -> ss_calling;	/* struct copy */
    ss -> ss_called.sa_addr = ts -> ts_called;	/* struct copy */
    if (s -> s_mask & SMASK_CN_CALLED) {
	if ((len = s -> s_calledlen)
		> sizeof ss -> ss_called.sa_selector)
	    len = sizeof ss -> ss_called.sa_selector;
	bcopy (s -> s_called, ss -> ss_called.sa_selector,
		ss -> ss_called.sa_selectlen = len);
    }
    sb -> sb_responding = ss -> ss_called;	/* struct copy */
    if ((ss -> ss_ssdusize = sb -> sb_tsdu_us - SSDU_MAGIC) < 0)
	ss -> ss_ssdusize = ts -> ts_tsdusize - SSDU_MAGIC;
    ss -> ss_qos = ts -> ts_qos;	/* struct copy */
    ss -> ss_qos.qos_sversion = sb -> sb_version + 1;
    ss -> ss_qos.qos_extended = (sb -> sb_flags & SB_EXPD) ? 1 : 0;
    copySPKTdata (s, ss);

    freespkt (s);

    return OK;

out2: ;
    freespkt(s);

out1: ;
    freesblk (sb);

    return NOTOK;
}

/*    S-CONNECT.RESPONSE */

#define	dotoken(requires,shift,bit,type) \
{ \
    if (sb -> sb_requirements & requires) \
	switch (sb -> sb_settings & (ST_MASK << shift)) { \
	    case ST_CALL_VALUE << shift: \
		switch (settings & (ST_MASK << shift)) { \
		    case ST_INIT_VALUE << shift: \
			settings &= ~(ST_MASK << shift); \
			settings |= ST_INIT_VALUE << shift; \
			break; \
 \
		    case ST_RESP_VALUE << shift: \
			settings &= ~(ST_MASK << shift); \
			settings |= ST_RESP_VALUE << shift; \
			sb -> sb_owned |= bit; \
			break; \
 \
		    default: \
			return ssaplose (si, SC_PARAMETER, NULLCP, \
				"improper choice of %s token setting", type); \
		} \
		break; \
 \
	    case ST_INIT_VALUE << shift: \
		if ((settings & (ST_MASK << shift)) == (ST_RSVD_VALUE << shift)) \
		    please |= bit; \
		settings &= ~(ST_MASK << shift); \
		settings |= ST_INIT_VALUE << shift; \
		break; \
 \
	    case ST_RESP_VALUE << shift: \
		settings &= ~(ST_MASK << shift); \
		settings |= ST_RESP_VALUE << shift; \
		sb -> sb_owned |= bit; \
		break; \
	} \
}
		
/*  */

int	SConnResponse (sd, ref, responding, status, requirements, settings,
	isn, data, cc, si)
int	sd;
struct SSAPref *ref;
struct SSAPaddr *responding;
int	status,
	requirements,
	settings,
	cc;
long	isn;
char   *data;
struct SSAPindication *si;
{
    int     result,
	    please;
    register struct ssapkt *s;
    register struct ssapblk *sb;

    if ((sb = findsblk (sd)) == NULL || (sb -> sb_flags & SB_CONN))
	return ssaplose (si, SC_PARAMETER, NULLCP, "invalid session descriptor");
    missingP (ref);
    refmuchP (ref);
    if (ref -> sr_vlen)
	return ssaplose (si, SC_PARAMETER, NULLCP, "bad format for reference");
#ifdef	notdef
    missingP (responding);
#endif
    if (responding)
	sb -> sb_responding = *responding;	/* struct copy */
    switch (status) {
	case SC_ACCEPT: 
	    if (requirements & ~SR_MYREQUIRE)
		return ssaplose (si, SC_PARAMETER, NULLCP,
			    "requirements settings not supported");
#ifdef	notdef		/* screwy session protocol... */
	    if (requirements & ~sb -> sb_requirements)
		return ssaplose (si, SC_PARAMETER, NULLCP,
			    "requirements settings not available");
#endif
	    if ((requirements & SR_HALFDUPLEX) && (requirements & SR_DUPLEX))
		return ssaplose (si, SC_PARAMETER, NULLCP,
			    "half-duplex and duplex services are incompatible");
	    if ((requirements & SR_EXCEPTIONS)
		    && !(requirements & SR_HALFDUPLEX))
		return ssaplose (si, SC_PARAMETER, NULLCP,
			    "exception service requires half-duplex service");
	    sb -> sb_requirements &= requirements;
	    sb -> sb_owned = 0, please = 0;
	    dotokens ();
	    if (sb -> sb_requirements
			& (SR_MINORSYNC | SR_MAJORSYNC | SR_RESYNC)) {
		if (!(sb -> sb_requirements & SR_ACTIVITY)
			|| isn != SERIAL_NONE)
		    if (SERIAL_MIN > isn || isn > SERIAL_MAX + 1)
			return ssaplose (si, SC_PARAMETER, NULLCP,
				"bad choice for initial serial number");
	    }
	    else
		if (isn != SERIAL_NONE)
		    return ssaplose (si, SC_PARAMETER, NULLCP,
			   "initial serial number invalid given requirements");
	    break;

	case SC_NOTSPECIFIED: 
	case SC_CONGESTION: 
	case SC_REJECTED: 
	    break;

	default: 
	    return ssaplose (si, SC_PARAMETER, NULLCP, "invalid result");
    }
    if (data == NULL)
	cc = 0;
    else
	if (cc > (sb -> sb_version < SB_VRSN2 ? SC_SIZE : ENCLOSE_MAX))
	    return ssaplose (si, SC_PARAMETER, NULLCP,
			     "too much initial user data, %d octets", cc);
    missingP (si);

    if (status != SC_ACCEPT) {
	if ((s = newspkt (SPDU_RF)) == NULL) {
	    (void) ssaplose (si, SC_CONGEST, NULLCP, "out of memory");
	    goto out1;
	}

	s -> s_mask |= SMASK_RF_REF;
	s -> s_rf_reference = *ref;	/* struct copy */
	if (status == SC_REJECTED) {
	    s -> s_mask |= SMASK_RF_REQ;
	    s -> s_rf_require = requirements;
	}
	if ((s -> s_rdata = malloc ((unsigned) (s -> s_rlen = 1 + cc)))
		== NULL) {
	    (void) ssaplose (si, SC_CONGEST, NULLCP, "out of memory");
	    goto out2;
	}
	*s -> s_rdata = status & 0xff;
	if (cc > 0)
	    bcopy (data, s -> s_rdata + 1, cc);
	result = refuse (sb, s, si);
	freesblk (sb);

	return (result != NOTOK && status != SC_ACCEPT ? OK : NOTOK);
    }

    if ((s = newspkt (SPDU_AC)) == NULL) {
	(void) ssaplose (si, SC_CONGEST, NULLCP, "out of memory");
	goto out1;
    }

    s -> s_mask |= SMASK_CN_REF | SMASK_CN_OPT | SMASK_CN_VRSN;
    s -> s_cn_reference = *ref;	/* struct copy */
    s -> s_options = CR_OPT_NULL;
    s -> s_cn_version = 1 << sb -> sb_version;

    if (isn != SERIAL_NONE) {
	s -> s_mask |= SMASK_CN_ISN;
	s -> s_isn = isn;
    }

    if (sb -> sb_tsdu_us || sb -> sb_tsdu_them) {
	s -> s_mask |= SMASK_CN_TSDU;
	s -> s_tsdu_resp = GET_TSDU_SIZE (sb -> sb_tsdu_us);
	s -> s_tsdu_init = GET_TSDU_SIZE (sb -> sb_tsdu_them);
    }

    s -> s_mask |= SMASK_CN_REQ;
    if ((s -> s_cn_require = sb -> sb_requirements) & SR_TOKENS) {
	s -> s_mask |= SMASK_CN_SET;
	s -> s_settings = settings;
    }
    if (please) {
	s -> s_mask |= SMASK_AC_TOKEN;
	s -> s_ac_token = please;
    }
    if (responding) {
	s -> s_mask |= SMASK_CN_CALLED;
	bcopy (sb -> sb_responding.sa_selector, s -> s_called,
		s -> s_calledlen = sb -> sb_responding.sa_selectlen);
    }

    if (cc > 0) {
	s -> s_mask |= SMASK_UDATA_PGI;
	s -> s_udata = data, s -> s_ulen = cc;
    }
    else
	s -> s_udata = NULL, s -> s_ulen = 0;
    if ((result = spkt2sd (s, sb -> sb_fd, 0, si)) == NOTOK)
	freesblk (sb);
    else
	sb -> sb_flags |= SB_CONN;
    s -> s_mask &= ~SMASK_UDATA_PGI;
    s -> s_udata = NULL, s -> s_ulen = 0;

    freespkt(s);
    
    return result;
    
out2: ;
    freespkt (s);
out1: ;
    freesblk (sb);

    return NOTOK;
}

#undef	dotoken

/*  */

static int  refuse (sb, s, si)
register struct ssapblk *sb;
register struct ssapkt *s;
register struct SSAPindication *si;
{
    int     result;
    struct TSAPdata txs;
    register struct TSAPdata   *tx = &txs;
    struct TSAPdisconnect   tds;
    register struct TSAPdisconnect *td = &tds;

    s -> s_mask |= SMASK_RF_DISC;
    s -> s_rf_disconnect |= RF_DISC_RELEASE;

    result = spkt2sd (s, sb -> sb_fd, sb -> sb_flags & SB_EXPD ? 1 : 0, si);

    freespkt (s);
    if (result == NOTOK)
	return NOTOK;

    if (ses_rf_timer >= 0)
	switch (TReadRequest (sb -> sb_fd, tx, ses_rf_timer, td)) {
	    case OK:
	    default: 		/* what could this be? */
	        TXFREE (tx);
		break;

	    case NOTOK:
		sb -> sb_fd = NOTOK;
		break;
	}

    return OK;
}

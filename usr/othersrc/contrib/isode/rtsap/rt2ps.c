/* rt2ps.c - RTPM: AcSAP/PSAP interface */

#ifndef	lint
static char *rcsid = "$Header: /f/osi/rtsap/RCS/rt2ps.c,v 7.7 91/02/22 09:42:15 mrose Interim $";
#endif

/* 
 * $Header: /f/osi/rtsap/RCS/rt2ps.c,v 7.7 91/02/22 09:42:15 mrose Interim $
 *
 *
 * $Log:	rt2ps.c,v $
 * Revision 7.7  91/02/22  09:42:15  mrose
 * Interim 6.8
 * 
 * Revision 7.6  91/01/24  14:50:03  mrose
 * update
 * 
 * Revision 7.5  90/11/21  11:32:32  mrose
 * sun
 * 
 * Revision 7.4  90/10/23  20:43:57  mrose
 * update
 * 
 * Revision 7.3  90/07/27  08:47:36  mrose
 * update
 * 
 * Revision 7.2  90/07/01  21:06:46  mrose
 * pepsy
 * 
 * Revision 6.2  89/06/23  11:28:32  mrose
 * touch-up
 * 
 * Revision 6.1  89/05/31  15:02:22  mrose
 * sek
 * 
 * Revision 6.0  89/03/18  23:43:06  mrose
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
#include "rtpkt.h"
#include "tailor.h"

/*    DATA */

int	psDATAser (), psTOKENser (), psSYNCser (), psACTIVITYser (),
	psREPORTser (),	psFINISHser (), psABORTser ();


long	time ();

/*  */

int	rt2pspturn (acb, priority, rti)
register struct assocblk *acb;
int	priority;
register struct RtSAPindication *rti;
{
    int     result;
    PE	    pe;
    struct PSAPindication   pis;
    struct PSAPindication  *pi = &pis;
    struct PSAPabort   *pa = &pi -> pi_abort;

    if (!(acb -> acb_flags & ACB_TWA))
	return rtsaplose (rti, RTS_OPERATION, NULLCP,
		"mode of association is monologue");
    if (acb -> acb_flags & ACB_TURN)
	return rtsaplose (rti, RTS_OPERATION, NULLCP, "turn owned by you");

/* begin RTTP APDU */
    if ((pe = int2prim (priority)) == NULLPE)
	return rtsaplose (rti, RTS_CONGEST, NULLCP, NULLCP);
    pe -> pe_context = acb -> acb_rtsid;
/* end RTTP APDU */

    PLOGP (rtsap_log,RTS_RTSE__apdus, pe, "RTTPapdu", 0);

    result = PPTokenRequest (acb -> acb_fd, ST_DAT_TOKEN, &pe, 1, pi);

    pe_free (pe);

    if (result == NOTOK) {
	(void) ps2rtslose (acb, rti, "PPTokenRequest", pa);
	freeacblk (acb);
    }

    return result;
}

/*  */

int	rt2psgturn (acb, rti)
register struct assocblk *acb;
register struct RtSAPindication *rti;
{
    struct PSAPindication   pis;
    struct PSAPindication *pi = &pis;
    struct PSAPabort  *pa = &pi -> pi_abort;

    if (!(acb -> acb_flags & ACB_TWA))
	return rtsaplose (rti, RTS_OPERATION, NULLCP,
		"mode of association is monologue");
    if (!(acb -> acb_flags & ACB_TURN))
	return rtsaplose (rti, RTS_OPERATION, NULLCP, "turn not owned by you");
    if (acb -> acb_flags & ACB_ACT)
	return rtsaplose (rti, RTS_OPERATION, NULLCP, "transfer in progress");

    if (PGControlRequest (acb -> acb_fd, pi) == NOTOK) {
	(void) ps2rtslose (acb, rti, "PGControlRequest", pa);
	freeacblk (acb);
	return NOTOK;
    }

    acb -> acb_flags &= ~(ACB_TURN | ACB_PLEASE);

    return OK;
}

/*  */

int	rt2pstrans (acb, data, secs, rti)
register struct assocblk *acb;
register PE	data;
int	secs;
register struct RtSAPindication *rti;
{
    register int    cc,
                    size;
    int     result,
            len;
    long    clock,
            limit;
    register char  *dp;
    char   *base;
    PE	    pe;
    struct SSAPactid    ids;
    register struct SSAPactid  *id = &ids;
    struct PSAPindication   pis;
    struct PSAPindication *pi = &pis;
    struct PSAPabort  *pa = &pi -> pi_abort;
    struct RtSAPabort *rta = &rti -> rti_abort;

    if (!(acb -> acb_flags & ACB_TURN))
	return rtsaplose (rti, RTS_OPERATION, NULLCP, "turn not owned by you");
    if (acb -> acb_flags & ACB_ACT)
	return rtsaplose (rti, RTS_OPERATION, NULLCP, "transfer in progress");

    if ((pe = int2prim (acb -> acb_actno)) == NULLPE)
	return rtsaplose (rti, RTS_CONGEST, NULLCP, NULLCP);
    result = pe2ssdu (pe, &base, &len);
    pe_free (pe);
    if (result == NOTOK)
	return rtsaplose (rti, RTS_CONGEST, NULLCP, NULLCP);
    bcopy (base, id -> sd_data, (int) (id -> sd_len = len));
    free (base);
    base = NULL;

    if (pe = pe_alloc (PE_CLASS_UNIV, PE_FORM_PRIM, PE_PRIM_OCTS)) {
	pe -> pe_inline = 1;
	pe -> pe_context = acb -> acb_rtsid;
    }
    else {
	(void) rtsaplose (rti, RTS_CONGEST, NULLCP, NULLCP);
	goto out;
    }
    if (PActStartRequest (acb -> acb_fd, id, NULLPEP, 0, pi) == NOTOK) {
	(void) ps2rtslose (acb, rti, "PActStartRequest", pa);
	goto out;
    }

    acb -> acb_flags |= ACB_ACT;

    if (data && pe2ssdu (data, &base, &len) == NOTOK) {
	(void) rtsaplose (rti, RTS_CONGEST, NULLCP, NULLCP);
	goto out;
    }

    result = OK;
    if (acb -> acb_ckpoint == 0) {
	if (data == NULLPE) {
	    if ((*acb -> acb_downtrans) (acb -> acb_fd, &base, &len, 0, 0L,
				 0L, rti) == NOTOK) {
bad_trans: ;
		if (PActDiscRequest (acb -> acb_fd, SP_LOCAL, pi) == NOTOK) {
		    (void) ps2rtslose (acb, rti, "PActDiscRequest", pa);
		    goto out;
		}
    		goto done;
	    }
	    if (len == 0) {
		base = NULL;
		goto done;
	    }
	}

	pe -> pe_prim = (PElementData) base, pe -> pe_len = (PElementLen) len;

	if (PDataRequest (acb -> acb_fd, &pe, 1, pi) == NOTOK) {
	    (void) ps2rtslose (acb, rti, "PDataRequest", pa);
	    goto out;
	}
    }
    else {
	size = acb -> acb_ckpoint << 10;	/* units of 1024 octets */
	if (acb -> acb_ssdusize >= 0x0100)	/* at least  256 octets */
	    size = min (size, acb -> acb_ssdusize);
	acb -> acb_ssn = acb -> acb_ack = 0L;
	if (secs != NOTOK) {
	    (void) time (&limit);
	    limit += secs;
	}

	if (data == NULLPE) {
	    if ((*acb -> acb_downtrans) (acb -> acb_fd, &base, &len, size,
					 acb -> acb_ssn, acb -> acb_ack,
					 rti) == NOTOK)
		goto bad_trans;
	    if (len == 0) {
		base = NULL;
		goto done;
	    }
	}

	dp = base, cc = min (len, size);
	pe -> pe_prim = (PElementData) dp, pe -> pe_len = (PElementLen) cc;
	if (PDataRequest (acb -> acb_fd, &pe, 1, pi) == NOTOK) {
	    (void) ps2rtslose (acb, rti, "PDataRequest", pa);
	    goto out;
	}

	for (dp += cc, len -= cc;
		data == NULLPE || len > 0;
		dp += cc, len -= cc) {
	    if (data == NULLPE && len == 0) {
		if ((*acb -> acb_downtrans) (acb -> acb_fd, &base, &len, size,
					    acb -> acb_ssn, acb -> acb_ack,
					     rti) == NOTOK)
		    goto bad_trans;
		if (len == 0) {
		    base = NULL;
		    break;
		}
		dp = base;
	    }

	    if (secs != NOTOK) {
		(void) time (&clock);
		if (limit < clock) {
		    result = NOTOK;
		    break;
		}
	    }

	    if (PMinSyncRequest (acb -> acb_fd, SYNC_CONFIRM,
			&acb -> acb_ssn, NULLPEP, 0, pi) == NOTOK) {
		(void) ps2rtslose (acb, rti, "PMinSyncRequest", pa);
		goto out;
	    }

	    if (acb -> acb_ssn - acb -> acb_ack > acb -> acb_window) {
		do {
		    if (RtWaitRequestAux (acb, NOTOK, 1, rti) == NOTOK) {
			if (RTS_FATAL (rta -> rta_reason))
			    acb = NULLACB;
			goto out;
		    }
		}
		while (acb -> acb_ssn - acb -> acb_ack > acb -> acb_window);

#ifdef	notdef
	    /* avoid silly window syndrome */
		while (acb -> acb_ssn != acb -> acb_ack)
		    if (RtWaitRequestAux (acb, OK, 1, rti) == NOTOK)
			if (rta -> rta_reason != RTS_TIMER) {
			    if (RTS_FATAL (rta -> rta_reason))
				acb = NULLACB;
			    goto out;
			}
			else
			    break;
#endif
	    }

	    cc = min (len, size);
	    pe -> pe_prim = (PElementData) dp, pe -> pe_len = cc;
	    if (PDataRequest (acb -> acb_fd, &pe, 1, pi) == NOTOK) {
		(void) ps2rtslose (acb, rti, "PDataRequest", pa);
		goto out;
	    }
	}
    }
    if (data)
	free (base);
    base = NULL;

done: ;
    switch (result) {
	case OK: 
	    if (PActEndRequest (acb -> acb_fd, &acb -> acb_ssn, NULLPEP, 0,
			pi) == NOTOK) {
		(void) ps2rtslose (acb, rti, "PActEndRequest", pa);
		goto out;
	    }
	    break;

	default: 
	    acb -> acb_flags |= ACB_TIMER;
	    if (PActDiscRequest (acb -> acb_fd, SP_LOCAL, pi) == NOTOK) {
		(void) ps2rtslose (acb, rti, "PActDiscRequest", pa);
		goto out;
	    }
	    break;
    }

    while (acb -> acb_flags & ACB_ACT)
	if (RtWaitRequestAux (acb, NOTOK, 1, rti) == NOTOK) {
	    if (RTS_FATAL (rta -> rta_reason))
		acb = NULLACB;
	    goto out;
	}

    acb -> acb_flags &= ~ACB_TIMER;
    acb -> acb_actno++;

    if (pe)
	pe_free (pe);

    return result;

out: ;
    if (data && base)
	free (base);
    if (pe)
	pe_free (pe);
    if (acb)
	freeacblk (acb);

    return NOTOK;
}

/*  */

int	rt2pswait (acb, secs, trans, rti)
register struct assocblk *acb;
int     secs,
	trans;
register struct RtSAPindication *rti;
{
    int     result;
    struct PSAPdata pxs;
    register struct PSAPdata   *px = &pxs;
    struct PSAPindication   pis;
    register struct PSAPindication *pi = &pis;

    for (;;) {
	switch (result = PReadRequest (acb -> acb_fd, px, secs, pi)) {
	    case NOTOK: 
		return doPSabort (acb, &pi -> pi_abort, rti);

	    case OK: 
		if (doPSdata (acb, px, rti) == NOTOK)
		    return NOTOK;
		continue;

	    case DONE: 
		switch (pi -> pi_type) {
		    case PI_TOKEN: 
			if ((result = doPStoken (acb, &pi -> pi_token, trans,
					rti)) != OK)
			    return result;
			continue;

		    case PI_SYNC: 
			if ((result = doPSsync (acb, &pi -> pi_sync, rti)) != OK
				|| trans)
			    return result;
			continue;

		    case PI_ACTIVITY: 
			if ((result = doPSactivity (acb, &pi -> pi_activity, rti)) != OK
				|| trans)
			    return (result != DONE ? result : OK);
			continue;

		    case PI_REPORT: 
			if (doPSreport (acb, &pi -> pi_report, rti) == NOTOK)
			    return NOTOK;
			continue;

		    case PI_FINISH: 
			return doPSfinish (acb, &pi -> pi_finish, rti);

		    default: 
			(void) rtpktlose (acb, rti, RTS_PROTOCOL, NULLCP,
				"unknown indication (0x%x) from presentation",
				pi -> pi_type);
			break;
		}
		break;

	    default: 
		(void) rtpktlose (acb, rti, RTS_PROTOCOL, NULLCP,
			"unexpected return from PReadRequest=%d", result);
		break;
	}
	break;
    }

    freeacblk (acb);
    return NOTOK;
}

/*    define vectors for INDICATION events */

#define	e(i)	(indication ? (i) : NULLIFP)


int	rt2psasync (acb, indication, rti)
register struct assocblk   *acb;
IFP	indication;
struct RtSAPindication *rti;
{
    struct PSAPindication   pis;
    struct PSAPindication *pi = &pis;
    struct PSAPabort  *pa = &pi -> pi_abort;

    if (acb -> acb_rtsindication = indication)
	acb -> acb_flags |= ACB_ASYN;
    else
	acb -> acb_flags &= ~ACB_ASYN;

    if (PSetIndications (acb -> acb_fd, e (psDATAser), e (psTOKENser),
		e (psSYNCser), e (psACTIVITYser), e (psREPORTser),
		e (psFINISHser), e (psABORTser), pi) == NOTOK) {
	acb -> acb_flags &= ~ACB_ASYN;
	switch (pa -> pa_reason) {
	    case PC_WAITING: 
		return rtsaplose (rti, RTS_WAITING, NULLCP, NULLCP);

	    default: 
		(void) ps2rtslose (acb, rti, "PSetIndications", pa);
		freeacblk (acb);
		return NOTOK;
	}
    }

    return OK;
}

#undef	e

/*    map association descriptors for select() */

int	rt2psmask (acb, mask, nfds, rti)
register struct assocblk   *acb;
fd_set *mask;
int    *nfds;
struct RtSAPindication *rti;
{
    struct PSAPindication   pis;
    struct PSAPindication  *pi = &pis;
    struct PSAPabort   *pa = &pi -> pi_abort;

    if (PSelectMask (acb -> acb_fd, mask, nfds, pi) == NOTOK)
	switch (pa -> pa_reason) {
	    case PC_WAITING: 
		return rtsaplose (rti, RTS_WAITING, NULLCP, NULLCP);

	    default: 
		(void) ps2rtslose (acb, rti, "PSelectMask", pa);
		freeacblk (acb);
		return NOTOK;
	}

    return OK;
}

/*    protocol-level abort */

int	rt2pslose (acb, result)
register struct assocblk   *acb;
int	result;
{
    PE	    pe;
    struct AcSAPindication  acis;

/* begin RTAB APDU */
    if ((pe = pe_alloc (PE_CLASS_UNIV, PE_FORM_CONS, 22))
	    && set_add (pe, num2prim ((integer) result, PE_CLASS_CONT,
				      RTAB_REASON)) != NOTOK) {
	pe -> pe_context = acb -> acb_rtsid;

	PLOGP (rtsap_log,RTS_RTSE__apdus, pe, "RTABapdu", 0);

	(void) AcUAbortRequest (acb -> acb_fd, &pe, 1, &acis);
	pe_free (pe);
    }
/* end RTAB APDU */
}

/*    AcSAP interface */

int	acs2rtslose (acb, rti, event, aca)
register struct assocblk *acb;
register struct RtSAPindication *rti;
char   *event;
register struct AcSAPabort *aca;
{
    int     reason;
    char   *cp,
            buffer[BUFSIZ];

    if (event)
	SLOG (rtsap_log, LLOG_EXCEPTIONS, NULLCP,
	      (aca -> aca_cc > 0 ? "%s: %s [%*.*s]": "%s: %s", event,
	       AcErrString (aca -> aca_reason), aca -> aca_cc, aca -> aca_cc,
	      aca -> aca_data));

    cp = "";
    switch (aca -> aca_reason) {
	case ACS_ADDRESS: 
	    reason = RTS_ADDRESS;
	    break;

	case ACS_REFUSED:
	    reason = RTS_REFUSED;
	    break;

	case ACS_CONGEST: 
	    reason = RTS_CONGEST;
	    break;

	case ACS_PARAMETER:
	    reason = RTS_PARAMETER;
	    break;

	case ACS_OPERATION:
	    reason = RTS_OPERATION;
	    break;

	default: 
	    (void) sprintf (cp = buffer, " (%s at association control)",
		    AcErrString (aca -> aca_reason));
	case ACS_PRESENTATION:
	    reason = RTS_ACS;
	    break;
    }

    if (acb) {
	if (aca -> aca_cc > 0)
	    return rtpktlose (acb, rti, reason, NULLCP, "%*.*s%s",
		    aca -> aca_cc, aca -> aca_cc, aca -> aca_data, cp);
	else
	    return rtpktlose (acb, rti, reason, NULLCP, "%s", cp);
    }
    else
	if (aca -> aca_cc > 0)
	    return rtsaplose (rti, reason, NULLCP, "%*.*s%s",
		    aca -> aca_cc, aca -> aca_cc, aca -> aca_data, cp);
	else
	    return rtsaplose (rti, reason, NULLCP, "%s", cp);
}

/*  */

int	acs2rtsabort (acb, aca, rti)
register struct assocblk *acb;
register struct AcSAPabort *aca;
struct RtSAPindication *rti;
{
    int     result;
    PE	    pe;
    struct type_RTS_RTSE__apdus *rtpdu = NULL;
    struct type_RTS_RTABapdu	*prtab;

    if (aca -> aca_source != ACA_USER) {
	(void) acs2rtslose (acb, rti, NULLCP, aca);
	goto out;
    }

    if (aca -> aca_ninfo == 0) {
	(void) rtsaplose (rti, RTS_ABORTED, NULLCP, NULLCP);
	goto out;
    }

    pe = aca -> aca_info[0];
    /* acsap_abort = ABORT_PERM, acsap_data = NULLPE; */
    result = decode_RTS_RTSE__apdus (pe, 1, NULLIP, NULLVP, &rtpdu);

#ifdef	DEBUG
    if (result != NOTOK && (rtsap_log -> ll_events & LLOG_PDUS))
	pvpdu (rtsap_log, print_RTS_RTSE__apdus_P, pe, "RTABapdu", 1);
#endif
    if (result != NOTOK) {
	if (rtpdu -> offset != type_RTS_RTSE__apdus_rtab__apdu) {
	    (void) rtsaplose (rti, RTS_PROTOCOL, "Unexpected PDU");
	    ACAFREE (aca);
	    goto out;
	}
	prtab = rtpdu -> un.rtab__apdu;

	if (prtab->userdataAB != NULLPE)
	    (void) pe_extract (pe, prtab->userdataAB);
	else
	    pe = NULLPE;
    }
    ACAFREE (aca);

    if (result == NOTOK) {
	(void) rtsaplose (rti, RTS_PROTOCOL, "%s", PY_pepy);
	goto out;
    }
    if (prtab->abortReason) {
	result = prtab -> abortReason -> parm;
    } else {
	result = ABORT_PERM;
    }
    switch (result) {
	case ABORT_LSP: 
	case ABORT_TMP: 
	    result = RTS_REMOTE;
	    break;

	default: 
	    result = RTS_PROTOCOL;
	    break;

	case ABORT_USER:
	    result = RTS_ABORTED;
	    break;
    }
    if (result == RTS_ABORTED) {
	register struct RtSAPabort *rta = &rti -> rti_abort;

	rti -> rti_type = RTI_ABORT;
	bzero ((char *) rta, sizeof *rta);

	rta -> rta_peer = 1;
	rta -> rta_reason = RTS_ABORTED;
	rta -> rta_udata = prtab->userdataAB;
	prtab->userdataAB = NULLPE;
    }
    else {
	(void) rtsaplose (rti, result, NULLCP, NULLCP);
    }

out: ;
    if (rtpdu)
	free_RTS_RTSE__apdus (rtpdu);
    if (acb) {
	if (!(acb -> acb_flags & ACB_STICKY))
	    acb -> acb_fd = NOTOK;
	freeacblk (acb);
    }

    return NOTOK;
}

/*    PSAP interface */

static int  doPSdata (acb, px, rti)
register struct assocblk   *acb;
register struct PSAPdata *px;
struct RtSAPindication *rti;
{
    unsigned int    i;
    register char  *dp;
    register PE	    pe;
    struct PSAPindication   pis;
    register struct PSAPindication *pi = &pis;
    register struct PSAPabort  *pa = &pi -> pi_abort;

    pe = NULLPE;
    if (!(acb -> acb_flags & ACB_ACT)
	    || (acb -> acb_flags & ACB_TURN)
	    || px -> px_type != SX_NORMAL) {
	(void) rtpktlose (acb, rti, RTS_PROTOCOL, NULLCP,
		"unexpected data indication (0x%x)", px -> px_type);
	PXFREE (px);
	goto out;
    }

    pe = px -> px_info[0], px -> px_info[0] = NULLPE;
    PXFREE (px);

    if (acb -> acb_uptrans) {
	int	result;
	register struct qbuf *qb;
	
	if ((qb = prim2qb (pe)) == NULL)
	    goto congested;
	result = (*acb -> acb_uptrans) (acb -> acb_fd, SI_DATA, (caddr_t) qb,
					rti);
	qb_free (qb);
	if (result == NOTOK)
	    goto congested;
	goto done;
    }
    
    if (pe -> pe_form == PE_FORM_CONS && pe_pullup (pe) == NOTOK)
	goto congested;

    if (acb -> acb_len > 0) {
	i = acb -> acb_len + pe -> pe_len;
	if (acb -> acb_realbase) {
	    if ((dp = malloc (i)) == NULL) {
	congested: ;
		if (PUReportRequest (acb -> acb_fd, SP_LOCAL, NULLPEP, 0, pi)
			== NOTOK) {
		    (void) ps2rtslose (acb, rti, "PUReportRequest", pa);
		    goto out;
		}
		FREEACB (acb);
		return OK;
	    }
	    bcopy (acb -> acb_base, dp, acb -> acb_len);
	    free (acb -> acb_realbase), acb -> acb_realbase = NULL;
	}
	else
	    if ((dp = realloc (acb -> acb_base, i)) == NULL)
		goto congested;
	bcopy ((char *) pe -> pe_prim, dp + acb -> acb_len, pe -> pe_len);
	acb -> acb_base = dp;
	acb -> acb_len = i;
    }
    else {
        if (pe -> pe_inline) {
            if ((acb -> acb_realbase = acb -> acb_base =
					malloc ((unsigned int) pe -> pe_len))
		    == NULL)
		goto congested;
            bcopy (pe -> pe_prim, acb -> acb_base,
		   acb -> acb_len = pe -> pe_len);
        }
        else {
	    acb -> acb_base = (char *) pe -> pe_prim;
	    acb -> acb_len =  pe -> pe_len;
	    pe -> pe_prim = NULLPED, pe -> pe_len = 0;
	    acb -> acb_realbase = pe -> pe_realbase,
	        pe -> pe_realbase = NULLCP;
        }
    }
done: ;
    pe_free (pe);
    return OK;

out: ;
    if (pe)
	pe_free (pe);

    freeacblk (acb);
    return NOTOK;
}

/*  */

static int  doPStoken (acb, pt, trans, rti)
register struct assocblk   *acb;
register struct PSAPtoken *pt;
int	trans;
struct RtSAPindication *rti;
{
    register PE	    pe;
    struct PSAPindication   pis;
    register struct PSAPindication *pi = &pis;
    register struct PSAPabort  *pa = &pi -> pi_abort;
    struct type_RTS_RTSE__apdus *rtpdu;
    struct type_RTS_RTTPapdu *prttp;

    if (acb -> acb_flags & ACB_TWA)
	switch (pt -> pt_type) {
	    case ST_CONTROL: 
		if (acb -> acb_flags & ACB_ACT)
		    break;

		PTFREE (pt);

		acb -> acb_owned = pt -> pt_owned;
		acb -> acb_flags |= ACB_TURN;

		rti -> rti_type = RTI_TURN;
		{
		    register struct RtSAPturn  *rtu = &rti -> rti_turn;

		    rtu -> rtu_please = 0;
		}
		return DONE;

	    case ST_PLEASE: 
		pe = pt -> pt_info[0];
		if (decode_RTS_RTSE__apdus (pe, 1, NULLIP, NULLVP, &rtpdu) == NOTOK) {
		    (void) pylose ();
		    goto out;
		}

		PLOGP (rtsap_log,RTS_RTSE__apdus, pe, "RTTPapdu", 1);
		if (rtpdu -> offset != type_RTS_RTSE__apdus_rttp__apdu) {
		    (void) rtpktlose (acb, rti, RTS_PROTOCOL, NULLCP,
				      "unexpected PDU");
		    free_RTS_RTSE__apdus (rtpdu);
		    goto out;
		}
		prttp = rtpdu -> un.rttp__apdu;
		PTFREE (pt);

		if (trans) {
		    if (acb -> acb_downtrans) {
			if ((*acb -> acb_downtrans) (acb -> acb_fd, NULLVP,
	    /* surely this should be rtsap_priority NULLIP, acsap_priority,*/
						    NULLIP, prttp -> parm,
						    0L, 0L, rti) == NOTOK
			        && PActIntrRequest (acb -> acb_fd, SP_LOCAL,
						    pi) == NOTOK) {
			    (void) ps2rtslose (acb, rti, "PActIntrRequest",pa);
			    free_RTS_RTSE__apdus (rtpdu);
			    goto out;
			}
		    }
		    else {
			acb -> acb_flags |= ACB_PLEASE;
			acb -> acb_priority = prttp -> parm;
		    }
		    free_RTS_RTSE__apdus (rtpdu);
		    return OK;
		}

		rti -> rti_type = RTI_TURN;
		{
		    register struct RtSAPturn  *rtu = &rti -> rti_turn;

		    rtu -> rtu_please = 1;
		    rtu -> rtu_priority = prttp -> parm;
		}
		free_RTS_RTSE__apdus (rtpdu);
		return DONE;

	    default: 
		break;
	}
    (void) rtpktlose (acb, rti, RTS_PROTOCOL, NULLCP,
	    "unexpected token indication (0x%x)", pt -> pt_type);

out: ;
    PTFREE (pt);
    freeacblk (acb);

    return NOTOK;
}

/*  */

static int  doPSsync (acb, pn, rti)
register struct assocblk   *acb;
register struct PSAPsync *pn;
struct RtSAPindication *rti;
{
    struct PSAPindication   pis;
    register struct PSAPindication *pi = &pis;
    register struct PSAPabort  *pa = &pi -> pi_abort;

    PNFREE (pn);

    if (acb -> acb_flags & ACB_ACT)
	switch (pn -> pn_type) {
	    case SN_MINORIND: 	/* always confirm it */
		if (acb -> acb_flags & ACB_TURN)
		    break;
		if (acb -> acb_uptrans) {
		    if ((*acb -> acb_uptrans) (acb -> acb_fd, SI_SYNC,
					       (caddr_t) pn, rti) == NOTOK) {
			if (PUReportRequest (acb -> acb_fd, SP_LOCAL,
					     NULLPEP, 0, pi) == NOTOK) {
			    (void) ps2rtslose (acb, rti, "PUReportRequest",pa);
			    goto out;
			}
			return OK;
		    }
		}
		if (PMinSyncResponse (acb -> acb_fd, pn -> pn_ssn,
			    NULLPEP, 0, pi) == NOTOK) {
		    (void) ps2rtslose (acb, rti, "PMinSyncResponse", pa);
		    goto out;
		}
		return OK;

	    case SN_MINORCNF: 
		if (!(acb -> acb_flags & ACB_TURN))
		    break;
		acb -> acb_ack = pn -> pn_ssn;
		return OK;

	    default: 
		break;
	}
    (void) rtpktlose (acb, rti, RTS_PROTOCOL, NULLCP,
	    "unexpected sync indication (0x%x)", pn -> pn_type);

out: ;
    freeacblk (acb);

    return NOTOK;
}

/*  */

static int  doPSactivity (acb, pv, rti)
register struct assocblk   *acb;
register struct PSAPactivity *pv;
struct RtSAPindication *rti;
{
    int     result;
    register PE	    pe;
    struct PSAPindication   pis;
    register struct PSAPindication *pi = &pis;
    register struct PSAPabort  *pa = &pi -> pi_abort;

    PVFREE (pv);

    switch (pv -> pv_type) {
	case SV_START: 
	    if (acb -> acb_flags & (ACB_ACT | ACB_TURN))
		break;
	    if (acb -> acb_uptrans) {
		if ((*acb -> acb_uptrans) (acb -> acb_fd, SI_ACTIVITY,
					   (caddr_t) pv, rti) == NOTOK) {
		    if (PUReportRequest (acb -> acb_fd, SP_LOCAL,
					 NULLPEP, 0, pi) == NOTOK) {
			(void) ps2rtslose (acb, rti, "PUReportRequest", pa);
			goto out;
		    }
		    return OK;
		}
	    }
	    acb -> acb_flags |= ACB_ACT;
	    return OK;

	case SV_RESUME: 	/* XXX: will support this later */
	    if (acb -> acb_flags & (ACB_ACT | ACB_TURN))
		break;
	    if (PUReportRequest (acb -> acb_fd, SP_PROCEDURAL, NULLPEP, 0,
			pi) == NOTOK) {
		(void) ps2rtslose (acb, rti, "PUReportRequest", pa);
		goto out;
	    }
	    acb -> acb_flags |= ACB_ACT;
	    return OK;

	case SV_INTRIND: 
	case SV_DISCIND: 
	    if (!(acb -> acb_flags & ACB_ACT)
		    || (acb -> acb_flags & ACB_TURN))
		break;
	    if (acb -> acb_uptrans)
		(void) (*acb -> acb_uptrans) (acb -> acb_fd, SI_ACTIVITY,
					      (caddr_t) pv, rti);
	    if ((pv -> pv_type == SV_INTRIND
			? PActIntrResponse (acb -> acb_fd, pi)
			: PActDiscResponse (acb -> acb_fd, pi)) == NOTOK) {
		(void) ps2rtslose (acb, rti, pv -> pv_type == SV_INTRIND
			? "PActIntrResponse" : "PActDiscResponse", pa);
		goto out;
	    }
	    FREEACB (acb);
	    acb -> acb_flags &= ~ACB_ACT;
	    return OK;

	case SV_INTRCNF: 
	case SV_DISCCNF: 
	    if (!(acb -> acb_flags & ACB_ACT)
		    || !(acb -> acb_flags & ACB_TURN))
		break;
	    acb -> acb_flags &= ~ACB_ACT;
	    (void) rtsaplose (rti, acb -> acb_flags & ACB_TIMER ? RTS_TIMER
		    : RTS_TRANSFER, NULLCP, NULLCP);
	    return OK;

	case SV_ENDIND: 
	    if (!(acb -> acb_flags & ACB_ACT)
		    || (acb -> acb_flags & ACB_TURN))
		break;
	    if (acb -> acb_uptrans) {
		if ((*acb -> acb_uptrans) (acb -> acb_fd, SI_ACTIVITY,
					   (caddr_t) pv, rti) == NOTOK) {
		    if (PUReportRequest (acb -> acb_fd, SP_LOCAL, NULLPEP, 0,
					 pi) == NOTOK) {
			(void) ps2rtslose (acb, rti, "PUReportRequest", pa);
			goto out;
		    }

		    return OK;
		}

		pe = NULLPE;
		goto end_it;
	    }

	    if (acb -> acb_base) {
		if (pe = ssdu2pe (acb -> acb_base, acb -> acb_len,
				  acb -> acb_realbase ? acb -> acb_realbase
				  		      : acb -> acb_base,
				  &result))
		    acb -> acb_realbase = acb -> acb_base = NULL;
	    }
	    else
		pe = NULLPE, result = PS_ERR_EOF;
	    FREEACB (acb);
	    if (pe == NULLPE) {
		if (result != PS_ERR_NMEM) {
		    (void) rtpktlose (acb, rti, RTS_PROTOCOL, NULLCP, "%s",
			    ps_error (result));
		    goto out;
		}
		if (PUReportRequest (acb -> acb_fd, SP_LOCAL, NULLPEP, 0, pi)
			== NOTOK) {
		    (void) ps2rtslose (acb, rti, "PUReportRequest", pa);
		    goto out;
		}
		return OK;
	    }
end_it: ;
	    if (PActEndResponse (acb -> acb_fd, NULLPEP, 0, pi) == NOTOK) {
		(void) ps2rtslose (acb, rti, "PActEndResponse", pa);
		if (pe)
		    pe_free (pe);
		goto out;
	    }
	    acb -> acb_flags &= ~ACB_ACT;

	    rti -> rti_type = RTI_TRANSFER;
	    {
		register struct RtSAPtransfer  *rtt = &rti -> rti_transfer;

		rtt -> rtt_data = pe;
	    }
	    return DONE;

	case SV_ENDCNF: 
	    if (!(acb -> acb_flags & ACB_ACT)
		    || !(acb -> acb_flags & ACB_TURN))
		break;
	    acb -> acb_flags &= ~ACB_ACT;
	    return OK;

	default: 
	    break;
    }
    (void) rtpktlose (acb, rti, RTS_PROTOCOL, NULLCP,
	    "unexpected activity indication (0x%x)", pv -> pv_type);

out: ;
    freeacblk (acb);
    return NOTOK;
}

/*  */

static int  doPSreport (acb, pp, rti)
register struct assocblk   *acb;
register struct PSAPreport *pp;
struct RtSAPindication *rti;
{
    struct PSAPindication   pis;
    register struct PSAPindication *pi = &pis;
    register struct PSAPabort  *pa = &pi -> pi_abort;

    PPFREE (pp);

    if (!pp -> pp_peer) {
	if (!(acb -> acb_flags & ACB_ACT))
	    goto out2;
	if (!(acb -> acb_flags & ACB_TURN))
	    return OK;

/* XXX: should try lots of things here, based on how many checkpoints have
	been acknowledged, but, for now we'll treate everything as severe... */

	(void) rtpktlose (acb, rti, RTS_PROTOCOL, NULLCP,
		"unrecoverable provider-initiated exception report");
    }

    if ((acb -> acb_flags & ACB_ACT)
	    || !(acb -> acb_flags & ACB_TURN)) {
out2: ;
	(void) rtpktlose (acb, rti, RTS_PROTOCOL, NULLCP,
		"unexpected exception report indication (0x%x)",
		pp -> pp_peer);
	goto out1;
    }

/* XXX: should try lots of things here, based on pp_reason,
	but, for now we'll treat everything as SP_NOREASON... */

    if (acb -> acb_uptrans)
	(void) (*acb -> acb_uptrans) (acb -> acb_fd, SI_REPORT,
				      (caddr_t) pp, rti);
    if (PActDiscRequest (acb -> acb_fd, SP_NOREASON, pi) != NOTOK)
	return OK;
    (void) ps2rtslose (acb, rti, "PActDiscRequest", pa);

out1: ;
    freeacblk (acb);
    return NOTOK;
}

/*  */

static int  doPSfinish (acb, pf, rti)
register struct assocblk   *acb;
struct PSAPfinish *pf;
struct RtSAPindication *rti;
{
    struct AcSAPindication acis;
    register struct AcSAPabort *aca = &acis.aci_abort;

    if (((acb -> acb_flags & ACB_INIT) && (acb -> acb_flags & ACB_TWA))
	    || (acb -> acb_flags & ACB_TURN)) {
	(void) rtpktlose (acb, rti, RTS_PROTOCOL, NULLCP,
		"association management botched");
	PFFREE (pf);
	goto out;
    }

    if (acb -> acb_flags & ACB_ACT) {
	(void) rtpktlose (acb, rti, RTS_PROTOCOL, NULLCP,
		"unexpected release indication");
	PFFREE (pf);
	goto out;
    }

    rti -> rti_type = RTI_FINISH;
    {
	register struct AcSAPfinish *acf = &rti -> rti_finish;

	if (AcFINISHser (acb -> acb_fd, pf, &acis) == NOTOK)
	    return acs2rtslose (acb, rti, "AcFINISHser", aca);

	*acf = acis.aci_finish;	/* struct copy */
    }

    return DONE;

out: ;
    freeacblk (acb);
    return NOTOK;
}

/*  */

static int  doPSabort (acb, pa, rti)
register struct assocblk   *acb;
register struct PSAPabort *pa;
struct RtSAPindication *rti;
{
    struct AcSAPindication  acis;
    register struct AcSAPabort *aca = &acis.aci_abort;

    if (!pa -> pa_peer && pa -> pa_reason == PC_TIMER)
	return rtsaplose (rti, RTS_TIMER, NULLCP, NULLCP);

    if (AcABORTser (acb -> acb_fd, pa, &acis) == NOTOK) {
	(void) acs2rtslose (acb, rti, "AcABORTser", aca);
	if (!(acb -> acb_flags & ACB_STICKY))
	    acb -> acb_fd = NOTOK;
	freeacblk (acb);

	return NOTOK;
    }

    return acs2rtsabort (acb, aca, rti);
}

/*  */

static int  psDATAser (sd, px)
int	sd;
register struct PSAPdata *px;
{
    IFP	    handler;
    register struct assocblk   *acb;
    struct RtSAPindication  rtis;
    register struct RtSAPindication *rti = &rtis;

    if ((acb = findacblk (sd)) == NULL)
	return;
    handler = acb -> acb_rtsindication;

    if (doPSdata (acb, px, rti) != OK)
	(*handler) (sd, rti);
}

/*  */

static int  psTOKENser (sd, pt)
int	sd;
register struct PSAPtoken *pt;
{
    IFP	    handler;
    register struct assocblk   *acb;
    struct RtSAPindication  rtis;
    register struct RtSAPindication *rti = &rtis;

    if ((acb = findacblk (sd)) == NULL)
	return;
    handler = acb -> acb_rtsindication;

    if (doPStoken (acb, pt, 0, rti) != OK)
	(*handler) (sd, rti);
}

/*  */

static int  psSYNCser (sd, pn)
int	sd;
register struct PSAPsync *pn;
{
    IFP	    handler;
    register struct assocblk   *acb;
    struct RtSAPindication  rtis;
    register struct RtSAPindication *rti = &rtis;

    if ((acb = findacblk (sd)) == NULL)
	return;
    handler = acb -> acb_rtsindication;

    if (doPSsync (acb, pn, rti) != OK)
	(*handler) (sd, rti);
}

/*  */

static int  psACTIVITYser (sd, pv)
int	sd;
register struct PSAPactivity *pv;
{
    IFP	    handler;
    register struct assocblk   *acb;
    struct RtSAPindication  rtis;
    register struct RtSAPindication *rti = &rtis;

    if ((acb = findacblk (sd)) == NULL)
	return;
    handler = acb -> acb_rtsindication;

    if (doPSactivity (acb, pv, rti) != OK)
	(*handler) (sd, rti);
}

/*  */

static int  psREPORTser (sd, pp)
int	sd;
register struct PSAPreport *pp;
{
    IFP	    handler;
    register struct assocblk   *acb;
    struct RtSAPindication  rtis;
    register struct RtSAPindication *rti = &rtis;

    if ((acb = findacblk (sd)) == NULL)
	return;
    handler = acb -> acb_rtsindication;

    if (doPSreport (acb, pp, rti) != OK)
	(*handler) (sd, rti);
}

/*  */

static int  psFINISHser (sd, pf)
int	sd;
struct PSAPfinish *pf;
{
    IFP	    handler;
    register struct assocblk   *acb;
    struct RtSAPindication  rtis;
    register struct RtSAPindication *rti = &rtis;

    if ((acb = findacblk (sd)) == NULL)
	return;
    handler = acb -> acb_rtsindication;

    (void) doPSfinish (acb, pf, rti);

    (*handler) (sd, rti);
}

/*  */

static int  psABORTser (sd, pa)
int	sd;
register struct PSAPabort *pa;
{
    IFP	    handler;
    register struct assocblk   *acb;
    struct RtSAPindication  rtis;
    register struct RtSAPindication *rti = &rtis;

    if ((acb = findacblk (sd)) == NULL)
	return;
    handler = acb -> acb_rtsindication;

    (void) doPSabort (acb, pa, rti);

    (*handler) (sd, rti);
}

/*  */

int	ps2rtslose (acb, rti, event, pa)
register struct assocblk *acb;
register struct RtSAPindication *rti;
char   *event;
register struct PSAPabort *pa;
{
    int     reason;
    char   *cp,
            buffer[BUFSIZ];

    if (event)
	SLOG (rtsap_log, LLOG_EXCEPTIONS, NULLCP,
	      (pa -> pa_cc > 0 ? "%s: %s [%*.*s]": "%s: %s", event,
	       PErrString (pa -> pa_reason), pa -> pa_cc, pa -> pa_cc,
	       pa -> pa_data));

    cp = "";
    switch (pa -> pa_reason) {
	case PC_ADDRESS: 
	    reason = RTS_ADDRESS;
	    break;

	case PC_REFUSED:
	    reason = RTS_REFUSED;
	    break;

	case PC_CONGEST: 
	    reason = RTS_CONGEST;
	    break;

	default: 
	    (void) sprintf (cp = buffer, " (%s at presentation)",
		    PErrString (pa -> pa_reason));
	case PC_SESSION:
	    reason = RTS_PRESENTATION;
	    break;
    }

    if (pa -> pa_cc > 0)
	return rtpktlose (acb, rti, reason, NULLCP, "%*.*s%s",
		pa -> pa_cc, pa -> pa_cc, pa -> pa_data, cp);
    else
	return rtpktlose (acb, rti, reason, NULLCP, "%s", *cp ? cp + 1 : cp);
}


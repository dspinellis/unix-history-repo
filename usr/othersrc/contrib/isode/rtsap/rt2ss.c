/* rt2ss.c - RTPM: SSAP interface */

#ifndef	lint
static char *rcsid = "$Header: /f/osi/rtsap/RCS/rt2ss.c,v 7.5 91/02/22 09:42:24 mrose Interim $";
#endif

/* 
 * $Header: /f/osi/rtsap/RCS/rt2ss.c,v 7.5 91/02/22 09:42:24 mrose Interim $
 *
 *
 * $Log:	rt2ss.c,v $
 * Revision 7.5  91/02/22  09:42:24  mrose
 * Interim 6.8
 * 
 * Revision 7.4  90/11/21  11:32:36  mrose
 * sun
 * 
 * Revision 7.3  90/10/23  20:44:11  mrose
 * update
 * 
 * Revision 7.2  90/07/01  21:07:02  mrose
 * pepsy
 * 
 * Revision 6.2  89/06/23  11:28:36  mrose
 * touch-up
 * 
 * Revision 6.1  89/05/31  15:02:28  mrose
 * sek
 * 
 * Revision 6.0  89/03/18  23:43:15  mrose
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

#define	doSSabort	ss2rtsabort


int	ssDATAser (), ssTOKENser (), ssSYNCser (), ssACTIVITYser (),
	ssREPORTser (),	ssFINISHser (), ssABORTser ();


long	time ();

/*  */

int	rt2sspturn (acb, priority, rti)
register struct assocblk *acb;
int	priority;
register struct RtSAPindication *rti;
{
    int     result,
            len;
    char   *base;
    PE	    pe;
    struct SSAPindication   sis;
    struct SSAPindication *si = &sis;
    struct SSAPabort  *sa = &si -> si_abort;

    if (!(acb -> acb_flags & ACB_TWA))
	return rtsaplose (rti, RTS_OPERATION, NULLCP,
		"mode of association is monologue");
    if (acb -> acb_flags & ACB_TURN)
	return rtsaplose (rti, RTS_OPERATION, NULLCP, "turn owned by you");

/* begin Priority PSDU (pseudo) */
    if ((pe = int2prim (priority)) == NULLPE)
	return rtsaplose (rti, RTS_CONGEST, NULLCP, NULLCP);
/* end Priority PSDU */

    PLOGP (rtsap_log,OACS_Priority, pe, "Priority", 0);

    result = pe2ssdu (pe, &base, &len);
    pe_free (pe);
    if (result == NOTOK)
	return rtsaplose (rti, RTS_CONGEST, NULLCP, NULLCP);

    result = SPTokenRequest (acb -> acb_fd, ST_DAT_TOKEN, base, len, si);
    free (base);

    if (result == NOTOK) {
	(void) ss2rtslose (acb, rti, "SPTokenRequest", sa);
	freeacblk (acb);
    }

    return result;
}

/*  */

int	rt2ssgturn (acb, rti)
register struct assocblk *acb;
register struct RtSAPindication *rti;
{
    struct SSAPindication   sis;
    struct SSAPindication *si = &sis;
    struct SSAPabort  *sa = &si -> si_abort;

    if (!(acb -> acb_flags & ACB_TWA))
	return rtsaplose (rti, RTS_OPERATION, NULLCP,
		"mode of association is monologue");
    if (!(acb -> acb_flags & ACB_TURN))
	return rtsaplose (rti, RTS_OPERATION, NULLCP, "turn not owned by you");
    if (acb -> acb_flags & ACB_ACT)
	return rtsaplose (rti, RTS_OPERATION, NULLCP, "transfer in progress");

    if (SGControlRequest (acb -> acb_fd, si) == NOTOK) {
	(void) ss2rtslose (acb, rti, "SGControlRequest", sa);
	freeacblk (acb);
	return NOTOK;
    }

    acb -> acb_flags &= ~(ACB_TURN | ACB_PLEASE);

    return OK;
}

/*  */

int	rt2sstrans (acb, data, secs, rti)
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
    struct SSAPindication   sis;
    struct SSAPindication *si = &sis;
    struct SSAPabort  *sa = &si -> si_abort;
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

    if (SActStartRequest (acb -> acb_fd, id, NULLCP, 0, si) == NOTOK) {
	(void) ss2rtslose (acb, rti, "SActStartRequest", sa);
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
		if (SActDiscRequest (acb -> acb_fd, SP_LOCAL, si) == NOTOK) {
		    (void) ss2rtslose (acb, rti, "SActDiscRequest", sa);
		    goto out;
		}
		goto done;
	    }
	    if (len == 0) {
		base = NULL;
		goto done;
	    }
	}

	if (SDataRequest (acb -> acb_fd, base, len, si) == NOTOK) {
	    (void) ss2rtslose (acb, rti, "SDataRequest", sa);
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
	if (SDataRequest (acb -> acb_fd, dp, cc, si) == NOTOK) {
	    (void) ss2rtslose (acb, rti, "SDataRequest", sa);
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

	    if (SMinSyncRequest (acb -> acb_fd, SYNC_CONFIRM,
			&acb -> acb_ssn, NULLCP, 0, si) == NOTOK) {
		(void) ss2rtslose (acb, rti, "SMinSyncRequest", sa);
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
	    if (SDataRequest (acb -> acb_fd, dp, cc, si) == NOTOK) {
		(void) ss2rtslose (acb, rti, "SDataRequest", sa);
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
	    if (SActEndRequest (acb -> acb_fd, &acb -> acb_ssn, NULLCP, 0,
			si) == NOTOK) {
		(void) ss2rtslose (acb, rti, "SActEndRequest", sa);
		goto out;
	    }
	    break;

	default: 
	    acb -> acb_flags |= ACB_TIMER;
	    if (SActDiscRequest (acb -> acb_fd, SP_LOCAL, si) == NOTOK) {
		(void) ss2rtslose (acb, rti, "SActDiscRequest", sa);
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

    return result;

out: ;
    if (data && base)
	free (base);
    if (acb)
	freeacblk (acb);

    return NOTOK;
}

/*  */

int	rt2sswait (acb, secs, trans, rti)
register struct assocblk *acb;
int     secs,
	trans;
register struct RtSAPindication *rti;
{
    int     result;
    struct SSAPdata sxs;
    register struct SSAPdata   *sx = &sxs;
    struct SSAPindication   sis;
    register struct SSAPindication *si = &sis;

    for (;;) {
	switch (result = SReadRequest (acb -> acb_fd, sx, secs, si)) {
	    case NOTOK: 
		return doSSabort (acb, &si -> si_abort, rti);

	    case OK: 
		if (doSSdata (acb, sx, rti) == NOTOK)
		    return NOTOK;
		continue;

	    case DONE: 
		switch (si -> si_type) {
		    case SI_TOKEN: 
			if ((result = doSStoken (acb, &si -> si_token, trans,
					rti)) != OK)
			    return result;
			continue;

		    case SI_SYNC: 
			if ((result = doSSsync (acb, &si -> si_sync, rti)) != OK
				|| trans)
			    return result;
			continue;

		    case SI_ACTIVITY: 
			if ((result = doSSactivity (acb, &si -> si_activity, rti)) != OK
				|| trans)
			    return (result != DONE ? result : OK);
			continue;

		    case SI_REPORT: 
			if (doSSreport (acb, &si -> si_report, rti) == NOTOK)
			    return NOTOK;
			continue;

		    case SI_FINISH: 
			return doSSfinish (acb, &si -> si_finish, rti);

		    default: 
			(void) rtpktlose (acb, rti, RTS_PROTOCOL, NULLCP,
				"unknown indication (0x%x) from session",
				si -> si_type);
			break;
		}
		break;

	    default: 
		(void) rtpktlose (acb, rti, RTS_PROTOCOL, NULLCP,
			"unexpected return from SReadRequest=%d", result);
		break;
	}
	break;
    }

    freeacblk (acb);
    return NOTOK;
}

/*    define vectors for INDICATION events */

#define	e(i)	(indication ? (i) : NULLIFP)


int	rt2ssasync (acb, indication, rti)
register struct assocblk   *acb;
IFP	indication;
struct RtSAPindication *rti;
{
    struct SSAPindication   sis;
    struct SSAPindication *si = &sis;
    struct SSAPabort  *sa = &si -> si_abort;

    if (acb -> acb_rtsindication = indication)
	acb -> acb_flags |= ACB_ASYN;
    else
	acb -> acb_flags &= ~ACB_ASYN;

    if (SSetIndications (acb -> acb_fd, e (ssDATAser), e (ssTOKENser),
		e (ssSYNCser), e (ssACTIVITYser), e (ssREPORTser),
		e (ssFINISHser), e (ssABORTser), si) == NOTOK) {
	acb -> acb_flags &= ~ACB_ASYN;
	switch (sa -> sa_reason) {
	    case SC_WAITING: 
		return rtsaplose (rti, RTS_WAITING, NULLCP, NULLCP);

	    default: 
		(void) ss2rtslose (acb, rti, "SSetIndications", sa);
		freeacblk (acb);
		return NOTOK;
	}
    }

    return OK;
}

#undef	e

/*    map association descriptors for select() */

int	rt2ssmask (acb, mask, nfds, rti)
register struct assocblk   *acb;
fd_set *mask;
int    *nfds;
struct RtSAPindication *rti;
{
    struct SSAPindication   sis;
    struct SSAPindication  *si = &sis;
    struct SSAPabort   *sa = &si -> si_abort;

    if (SSelectMask (acb -> acb_fd, mask, nfds, si) == NOTOK)
	switch (sa -> sa_reason) {
	    case SC_WAITING: 
		return rtsaplose (rti, RTS_WAITING, NULLCP, NULLCP);

	    default: 
		(void) ss2rtslose (acb, rti, "SSelectMask", sa);
		freeacblk (acb);
		return NOTOK;
	}

    return OK;
}

/*    protocol-level abort */

int	rt2sslose (acb, result)
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

	PLOGP (rtsap_log,OACS_AbortInformation, pe, "AbortInformation",
	      0);


	pe_free (pe);
    }
/* end AbortInformation PSDU */

    (void) SUAbortRequest (acb -> acb_fd, base, len, &sis);
    if (!(acb -> acb_flags & ACB_STICKY))
	acb -> acb_fd = NOTOK;

    if (base)
	free (base);
}

/*    SSAP interface */

static int  doSSdata (acb, sx, rti)
register struct assocblk   *acb;
register struct SSAPdata *sx;
struct RtSAPindication *rti;
{
    register struct qbuf *qb;
    struct SSAPindication   sis;
    register struct SSAPindication *si = &sis;
    register struct SSAPabort  *sa = &si -> si_abort;

    if (!(acb -> acb_flags & ACB_ACT)
	    || (acb -> acb_flags & ACB_TURN)
	    || sx -> sx_type != SX_NORMAL) {
	(void) rtpktlose (acb, rti, RTS_PROTOCOL, NULLCP,
		"unexpected data indication (0x%x)", sx -> sx_type);
	goto out;
    }

    if (acb -> acb_uptrans) {
	if ((*acb -> acb_uptrans) (acb -> acb_fd, SI_DATA,
				   (caddr_t) &sx -> sx_qbuf, rti) == NOTOK)
	    goto congested;

	goto done;
    }

    if (acb -> acb_len > 0) {
	unsigned int    i;
	register char  *cp,
		       *dp;

	i = acb -> acb_len + sx -> sx_cc;
	if (acb -> acb_realbase) {
	    if ((dp = malloc (i)) == NULL) {
	congested: ;
		if (SUReportRequest (acb -> acb_fd, SP_LOCAL, NULLCP, 0,
			    si) == NOTOK) {
		    (void) ss2rtslose (acb, rti, "SUReportRequest", sa);
		    goto out;
		}
		FREEACB (acb);
		goto done;
	    }
	    bcopy (acb -> acb_base, dp, acb -> acb_len);
	    free (acb -> acb_realbase), acb -> acb_realbase = NULL;
	}
	else
	    if ((dp = realloc (acb -> acb_base, i)) == NULL)
		goto congested;

	cp = dp + acb -> acb_len;
	for (qb = sx -> sx_qbuf.qb_forw;
		qb != &sx -> sx_qbuf;
		qb = qb -> qb_forw)
	    if (qb -> qb_len) {
		bcopy (qb -> qb_data, cp, qb -> qb_len);
		cp += qb -> qb_len;
	    }
	acb -> acb_base = dp;
	acb -> acb_len = i;
    }
    else {
	if ((qb = sx -> sx_qbuf.qb_forw) != &sx -> sx_qbuf
		&& qb -> qb_forw == &sx -> sx_qbuf) {
	    remque (qb);

	    acb -> acb_realbase = (char *) qb;
	    acb -> acb_base = qb -> qb_data;
	}
	else
	    acb -> acb_base = qb2str (&sx -> sx_qbuf);

	acb -> acb_len = sx -> sx_cc;
    }
done: ;
    SXFREE (sx);

    return OK;

out: ;
    SXFREE (sx);

    freeacblk (acb);
    return NOTOK;
}

/*  */

static int  doSStoken (acb, st, trans, rti)
register struct assocblk   *acb;
register struct SSAPtoken *st;
int	trans;
struct RtSAPindication *rti;
{
    int     result;
    register PE	    pe;
    struct SSAPindication   sis;
    register struct SSAPindication *si = &sis;
    register struct SSAPabort  *sa = &si -> si_abort;
    struct type_OACS_Priority	*priority;

    if (acb -> acb_flags & ACB_TWA)
	switch (st -> st_type) {
	    case ST_CONTROL: 
		STFREE (st);
		if (acb -> acb_flags & ACB_ACT)
		    break;
		acb -> acb_owned = st -> st_owned;
		acb -> acb_flags |= ACB_TURN;

		rti -> rti_type = RTI_TURN;
		{
		    register struct RtSAPturn  *rtu = &rti -> rti_turn;

		    rtu -> rtu_please = 0;
		}
		return DONE;

	    case ST_PLEASE:
		pe = ssdu2pe (st -> st_data, st -> st_cc, NULLCP, &result);
		STFREE (st);
		if (pe == NULLPE) {
		    (void) rtpktlose (acb, rti, result != PS_ERR_NMEM
			    ? RTS_PROTOCOL : RTS_CONGEST, NULLCP,
			    ps_error (result));
		    goto out;
		}
		result = parse_OACS_Priority(pe, 1, NULLIP, NULLVP, &priority);

#ifdef	DEBUG
		if (result != NOTOK && (rtsap_log -> ll_events & LLOG_PDUS))
		    pvpdu (rtsap_log, print_OACS_Priority_P, pe, "Priority",
			   1);
#endif

		pe_free (pe);
		if (result == NOTOK) {
		    (void) pylose ();
		    free_OACS_Priority(priority);
		    goto out;
		}

		if (trans) {
		    if (acb -> acb_downtrans) {
			if ((*acb -> acb_downtrans) (acb -> acb_fd, NULLVP,
						    NULLIP, priority -> parm,
						    0L, 0L, rti) == NOTOK
			        && SActIntrRequest (acb -> acb_fd, SP_LOCAL,
						    si) == NOTOK) {
			    (void) ss2rtslose (acb, rti, "SActIntrRequest",sa);
			    free_OACS_Priority(priority);
			    goto out;
			}
		    }
		    else {
			acb -> acb_flags |= ACB_PLEASE;
			acb -> acb_priority = priority -> parm;
		    }
		    free_OACS_Priority(priority);
		    return OK;
		}

		rti -> rti_type = RTI_TURN;
		{
		    register struct RtSAPturn  *rtu = &rti -> rti_turn;

		    rtu -> rtu_please = 1;
		    rtu -> rtu_priority = priority -> parm;
		}
		free_OACS_Priority(priority);
		return DONE;

	    default: 
		break;
	}
    (void) rtpktlose (acb, rti, RTS_PROTOCOL, NULLCP,
	    "unexpected token indication (0x%x)", st -> st_type);

out: ;
    STFREE (st);

    freeacblk (acb);
    return NOTOK;
}

/*  */

static int  doSSsync (acb, sn, rti)
register struct assocblk   *acb;
register struct SSAPsync *sn;
struct RtSAPindication *rti;
{
    struct SSAPindication   sis;
    register struct SSAPindication *si = &sis;
    register struct SSAPabort  *sa = &si -> si_abort;

    SNFREE (sn);
    
    if (acb -> acb_flags & ACB_ACT)
	switch (sn -> sn_type) {
	    case SN_MINORIND: 	/* always confirm it */
		if (acb -> acb_flags & ACB_TURN)
		    break;
		if (acb -> acb_uptrans) {
		    if ((*acb -> acb_uptrans) (acb -> acb_fd, SI_SYNC,
					       (caddr_t) sn, rti) == NOTOK) {
			if (SUReportRequest (acb -> acb_fd, SP_LOCAL,
					     NULLCP, 0, si) == NOTOK) {
			    (void) ss2rtslose (acb, rti, "SUReportRequest",sa);
			    goto out;
			}
			return OK;
		    }
		}
		if (SMinSyncResponse (acb -> acb_fd, sn -> sn_ssn,
			    NULLCP, 0, si) == NOTOK) {
		    (void) ss2rtslose (acb, rti, "SMinSyncResponse", sa);
		    goto out;
		}
		return OK;

	    case SN_MINORCNF: 
		if (!(acb -> acb_flags & ACB_TURN))
		    break;
		acb -> acb_ack = sn -> sn_ssn;
		return OK;

	    default: 
		break;
	}
    (void) rtpktlose (acb, rti, RTS_PROTOCOL, NULLCP,
	    "unexpected sync indication (0x%x)", sn -> sn_type);

out: ;
    freeacblk (acb);

    return NOTOK;
}

/*  */

static int  doSSactivity (acb, sv, rti)
register struct assocblk   *acb;
register struct SSAPactivity *sv;
struct RtSAPindication *rti;
{
    int     result;
    register PE	    pe;
    struct SSAPindication   sis;
    register struct SSAPindication *si = &sis;
    register struct SSAPabort  *sa = &si -> si_abort;

    SVFREE (sv);

    switch (sv -> sv_type) {
	case SV_START: 
	    if (acb -> acb_flags & (ACB_ACT | ACB_TURN))
		break;
	    if (acb -> acb_uptrans) {
		if ((*acb -> acb_uptrans) (acb -> acb_fd, SI_ACTIVITY,
					   (caddr_t) sv, rti) == NOTOK) {
		    if (SUReportRequest (acb -> acb_fd, SP_LOCAL,
					 NULLCP, 0, si) == NOTOK) {
			(void) ss2rtslose (acb, rti, "SUReportRequest", sa);
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
	    if (SUReportRequest (acb -> acb_fd, SP_PROCEDURAL, NULLCP, 0,
			si) == NOTOK) {
		(void) ss2rtslose (acb, rti, "SUReportRequest", sa);
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
					      (caddr_t) sv, rti);
	    if ((sv -> sv_type == SV_INTRIND
			? SActIntrResponse (acb -> acb_fd, si)
			: SActDiscResponse (acb -> acb_fd, si)) == NOTOK) {
		(void) ss2rtslose (acb, rti, sv -> sv_type == SV_INTRIND
			? "SActIntrResponse" : "SActDiscResponse", sa);
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
					   (caddr_t) sv, rti) == NOTOK) {
		    if (SUReportRequest (acb -> acb_fd, SP_LOCAL, NULLCP, 0,
					 si) == NOTOK) {
			(void) ss2rtslose (acb, rti, "SUReportRequest", sa);
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
		if (SUReportRequest (acb -> acb_fd, SP_LOCAL, NULLCP, 0, si)
			== NOTOK) {
		    (void) ss2rtslose (acb, rti, "SUReportRequest", sa);
		    goto out;
		}
		return OK;
	    }

end_it: ;
	    if (SActEndResponse (acb -> acb_fd, NULLCP, 0, si) == NOTOK) {
		(void) ss2rtslose (acb, rti, "SActEndResponse", sa);
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
	    "unexpected activity indication (0x%x)", sv -> sv_type);

out: ;
    freeacblk (acb);
    return NOTOK;
}

/*  */

static int  doSSreport (acb, sp, rti)
register struct assocblk   *acb;
register struct SSAPreport *sp;
struct RtSAPindication *rti;
{
    struct SSAPindication   sis;
    register struct SSAPindication *si = &sis;
    register struct SSAPabort  *sa = &si -> si_abort;

    SPFREE (sp);

    if (!sp -> sp_peer) {
	if (!(acb -> acb_flags & ACB_ACT))
	    goto out2;

/* XXX: should try lots of things here, based on how many checkpoints have
	been acknowledged, but, for now we'll treate everything as severe... */

	(void) rtpktlose (acb, rti, RTS_PROTOCOL, NULLCP,
		"unrecoverable provider-initiated exception report");
	goto out1;
    }

    if (!(acb -> acb_flags & ACB_ACT)
	    || !(acb -> acb_flags & ACB_TURN)) {
out2: ;
	(void) rtpktlose (acb, rti, RTS_PROTOCOL, NULLCP,
		"unexpected exception report indication (0x%x)",
		sp -> sp_peer);
	goto out1;
    }

/* XXX: should try lots of things here, based on pp_reason,
	but, for now we'll treat everything as SP_NOREASON... */

    if (acb -> acb_uptrans)
	(void) (*acb -> acb_uptrans) (acb -> acb_fd, SI_REPORT,
				      (caddr_t) sp, rti);
    if (SActDiscRequest (acb -> acb_fd, SP_NOREASON, si) != NOTOK)
	return OK;
    (void) ss2rtslose (acb, rti, "SActDiscRequest", sa);

out1: ;
    freeacblk (acb);
    return NOTOK;
}

/*  */

/* ARGSUSED */

static int  doSSfinish (acb, sf, rti)
register struct assocblk   *acb;
struct SSAPfinish *sf;
struct RtSAPindication *rti;
{
    SFFREE (sf);

    if (((acb -> acb_flags & ACB_INIT) && (acb -> acb_flags & ACB_TWA))
	    || (acb -> acb_flags & ACB_TURN)) {
	(void) rtpktlose (acb, rti, RTS_PROTOCOL, NULLCP,
		"association management botched");
	goto out;
    }

    if (acb -> acb_flags & ACB_ACT) {
	(void) rtpktlose (acb, rti, RTS_PROTOCOL, NULLCP,
		"unexpected release indication");
	goto out;
    }

    acb -> acb_flags |= ACB_FINN;
    rti -> rti_type = RTI_CLOSE;
    {
	register struct RtSAPclose *rtc = &rti -> rti_close;

	bzero ((char *) rtc, sizeof *rtc);
    }
    return DONE;

out: ;
    freeacblk (acb);
    return NOTOK;
}

/*  */

int	ss2rtsabort (acb, sa, rti)
register struct assocblk   *acb;
register struct SSAPabort *sa;
struct RtSAPindication *rti;
{
    int     result;
    register PE	    pe;
    struct type_OACS_AbortInformation *pabi = 0;

    if (!sa -> sa_peer) {
	if (sa -> sa_reason == SC_TIMER)
	    return rtsaplose (rti, RTS_TIMER, NULLCP, NULLCP);

	(void) ss2rtslose (acb, rti, NULLCP, sa);
	goto out;
    }

    if (sa -> sa_cc == 0) {
	(void) rtsaplose (rti, RTS_ABORTED, NULLCP, NULLCP);
	goto out;
    }

    if ((pe = ssdu2pe (sa -> sa_info, sa -> sa_cc, NULLCP, &result))
	    == NULLPE) {
	(void) rtsaplose (rti, RTS_PROTOCOL, NULLCP, NULLCP);
	goto out;
    }
    /* acsap_abort = -1; */
    result = parse_OACS_AbortInformation (pe, 1, NULLIP, NULLVP, &pabi);

#ifdef	DEBUG
    if (result != NOTOK && (rtsap_log -> ll_events & LLOG_PDUS))
	pvpdu (rtsap_log, print_OACS_AbortInformation_P, pe,
	       "AbortInformation", 1);
#endif

    pe_free (pe);
    if (result == NOTOK) {
	(void) rtsaplose (rti, RTS_PROTOCOL, "%s", PY_pepy);
	free_OACS_AbortInformation (pabi);
	goto out;
    }
    if (pabi->member_OACS_6)
	result = pabi -> member_OACS_6 -> parm;
    else
	result = -1;
    switch (result) {
	case ABORT_LSP: 
	case ABORT_TMP: 
	    result = RTS_REMOTE;
	    break;

	default: 
	    result = RTS_PROTOCOL;
	    break;
    }
    (void) rtsaplose (rti, result, NULLCP, NULLCP);
    free_OACS_AbortInformation (pabi);

out: ;
    SAFREE (sa);
    if (!(acb -> acb_flags & ACB_STICKY))
	acb -> acb_fd = NOTOK;
    freeacblk (acb);

    return NOTOK;
}

/*  */

static int  ssDATAser (sd, sx)
int	sd;
register struct SSAPdata *sx;
{
    IFP	    handler;
    register struct assocblk   *acb;
    struct RtSAPindication  rtis;
    register struct RtSAPindication *rti = &rtis;

    if ((acb = findacblk (sd)) == NULL)
	return;
    handler = acb -> acb_rtsindication;

    if (doSSdata (acb, sx, rti) != OK)
	(*handler) (sd, rti);
}

/*  */

static int  ssTOKENser (sd, st)
int	sd;
register struct SSAPtoken *st;
{
    IFP	    handler;
    register struct assocblk   *acb;
    struct RtSAPindication  rtis;
    register struct RtSAPindication *rti = &rtis;

    if ((acb = findacblk (sd)) == NULL)
	return;
    handler = acb -> acb_rtsindication;

    if (doSStoken (acb, st, 0, rti) != OK)
	(*handler) (sd, rti);
}

/*  */

static int  ssSYNCser (sd, sn)
int	sd;
register struct SSAPsync *sn;
{
    IFP	    handler;
    register struct assocblk   *acb;
    struct RtSAPindication  rtis;
    register struct RtSAPindication *rti = &rtis;

    if ((acb = findacblk (sd)) == NULL)
	return;
    handler = acb -> acb_rtsindication;

    if (doSSsync (acb, sn, rti) != OK)
	(*handler) (sd, rti);
}

/*  */

static int  ssACTIVITYser (sd, sv)
int	sd;
register struct SSAPactivity *sv;
{
    IFP	    handler;
    register struct assocblk   *acb;
    struct RtSAPindication  rtis;
    register struct RtSAPindication *rti = &rtis;

    if ((acb = findacblk (sd)) == NULL)
	return;
    handler = acb -> acb_rtsindication;

    if (doSSactivity (acb, sv, rti) != OK)
	(*handler) (sd, rti);
}

/*  */

static int  ssREPORTser (sd, sp)
int	sd;
register struct SSAPreport *sp;
{
    IFP	    handler;
    register struct assocblk   *acb;
    struct RtSAPindication  rtis;
    register struct RtSAPindication *rti = &rtis;

    if ((acb = findacblk (sd)) == NULL)
	return;
    handler = acb -> acb_rtsindication;

    if (doSSreport (acb, sp, rti) != OK)
	(*handler) (sd, rti);
}

/*  */

static int  ssFINISHser (sd, sf)
int	sd;
struct SSAPfinish *sf;
{
    IFP	    handler;
    register struct assocblk   *acb;
    struct RtSAPindication  rtis;
    register struct RtSAPindication *rti = &rtis;

    if ((acb = findacblk (sd)) == NULL)
	return;
    handler = acb -> acb_rtsindication;

    (void) doSSfinish (acb, sf, rti);

    (*handler) (sd, rti);
}

/*  */

static int  ssABORTser (sd, sa)
int	sd;
register struct SSAPabort *sa;
{
    IFP	    handler;
    register struct assocblk   *acb;
    struct RtSAPindication  rtis;
    register struct RtSAPindication *rti = &rtis;

    if ((acb = findacblk (sd)) == NULL)
	return;
    handler = acb -> acb_rtsindication;

    (void) doSSabort (acb, sa, rti);

    (*handler) (sd, rti);
}

/*  */

int	ss2rtslose (acb, rti, event, sa)
register struct assocblk *acb;
register struct RtSAPindication *rti;
char   *event;
register struct SSAPabort *sa;
{
    int     reason;
    char   *cp,
            buffer[BUFSIZ];

    if (event)
	SLOG (rtsap_log, LLOG_EXCEPTIONS, NULLCP,
	      (sa -> sa_cc > 0 ? "%s: %s [%*.*s]": "%s: %s", event,
	       SErrString (sa -> sa_reason), sa -> sa_cc, sa -> sa_cc,
	       sa -> sa_data));

    cp = "";
    switch (sa -> sa_reason) {
	case SC_SSAPID: 
	case SC_SSUSER: 
	case SC_ADDRESS: 
	    reason = RTS_ADDRESS;
	    break;

	case SC_REFUSED:
	    reason = RTS_REFUSED;
	    break;

	case SC_CONGEST: 
	    reason = RTS_CONGEST;
	    break;

	default: 
	    (void) sprintf (cp = buffer, " (%s at session)",
		    SErrString (sa -> sa_reason));
	case SC_TRANSPORT:
	case SC_ABORT:
	    reason = RTS_SESSION;
	    break;
    }

    if (sa -> sa_cc > 0)
	return rtpktlose (acb, rti, reason, NULLCP, "%*.*s%s",
		sa -> sa_cc, sa -> sa_cc, sa -> sa_data, cp);
    else
	return rtpktlose (acb, rti, reason, NULLCP, "%s", *cp ? cp + 1 : cp);
}

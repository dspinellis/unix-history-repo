/* fd2tpkt.c - read/write a TPDU thru a socket */

#ifndef	lint
static char *rcsid = "$Header: /f/osi/tsap/RCS/fd2tpkt.c,v 7.3 91/03/09 11:58:04 mrose Exp $";
#endif

/* 
 * $Header: /f/osi/tsap/RCS/fd2tpkt.c,v 7.3 91/03/09 11:58:04 mrose Exp $
 *
 *
 * $Log:	fd2tpkt.c,v $
 * Revision 7.3  91/03/09  11:58:04  mrose
 * update
 * 
 * Revision 7.2  91/02/22  09:47:04  mrose
 * Interim 6.8
 * 
 * Revision 7.1  89/12/07  01:07:25  mrose
 * queued writes
 * 
 * Revision 7.0  89/11/23  22:30:26  mrose
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
#include "tpkt.h"
#include "tailor.h"

/*  */

struct tsapkt *fd2tpkt (fd, initfnx, readfnx)
int	fd;
IFP	initfnx,
	readfnx;
{
    register struct tsapkt *t;

    if ((t = newtpkt (0)) == NULL)
	return NULL;

    if ((t -> t_errno = fd2tpktaux (fd, t, initfnx, readfnx)) != OK) {
	if (t -> t_vdata != NULL)
	    free (t -> t_vdata), t -> t_vdata = NULL, t -> t_vlen = 0;

	if (t -> t_qbuf)
	    free ((char *) t -> t_qbuf), t -> t_qbuf = NULL;
    }

#ifdef	DEBUG
    if (tsap_log -> ll_events & LLOG_PDUS)
	tpkt2text (tsap_log, t, 1);
#endif

    return t;
}

/*  */

static int  fd2tpktaux (fd, t, initfnx, readfnx)
int	fd;
register struct tsapkt *t;
IFP	initfnx,
	readfnx;
{
    register int    code,
                    len,
                    vlen;
    register char  *vptr;

    if ((code = (*initfnx) (fd, t)) != OK)
	return code;
    if (t -> t_li > TPDU_MAXLEN (t))
	return DR_LENGTH;

    switch (TPDU_CODE (t)) {
	case TPDU_CR:
	case TPDU_CC:
	    if (t -> t_li < TPDU_MINLEN (t, CR))
		return DR_LENGTH;
	    if (readx (fd, (char *) &t -> t_cr, CR_SIZE (t), readfnx)
		    != CR_SIZE (t))
		return DR_NETWORK;

	    if (vlen = t -> t_vlen = t -> t_li - TPDU_MINLEN (t, CR)) {
		if ((vptr = t -> t_vdata = malloc ((unsigned) vlen)) == NULL)
		    return DR_CONGEST;
		if (readx (fd, t -> t_vdata, t -> t_vlen, readfnx)
			!= t -> t_vlen)
		    return DR_NETWORK;
		for (; vlen > 0; vptr += len, vlen -= len) {
		    int	    ilen;
		    
		    if (vlen < 2)
			return DR_LENGTH;
		    code = *vptr++ & 0xff;
		    len = *vptr++ & 0xff;
		    if ((vlen -= 2) < len)
			return DR_LENGTH;

		    switch (code) {
			case VDAT_TSAP_SRV:
			    if ((ilen = len) > sizeof t -> t_called)
				ilen = sizeof t -> t_called;
			    bcopy (vptr, t -> t_called,
					t -> t_calledlen = ilen);
			    break;

			case VDAT_TSAP_CLI:
			    if ((ilen = len) > sizeof t -> t_calling)
				ilen = sizeof t -> t_calling;
			    bcopy (vptr, t -> t_calling,
					t -> t_callinglen = ilen);
			    break;

			case VDAT_SIZE:
			    if (len != 1)
				return DR_LENGTH;
			    t -> t_tpdusize = *vptr & 0xff;
			    break;

			case VDAT_OPTIONS:
			    if (len != 1)
				return DR_LENGTH;
			    t -> t_options = *vptr & 0xff;
			    break;

			case VDAT_ALTERNATE:
			    {
				register int i;
				register char *ap;

				for (ap = vptr, i = len; i > 0; ap++, i--)
				    t -> t_cr.cr_alternate |=
						1 << ((*ap >> 4) & 0x0f);
			    }
			    break;

			case VDAT_VRSN:
			case VDAT_SECURITY:
			case VDAT_CHECKSUM:
			case VDAT_ACKTIME:
			case VDAT_THROUGHPUT:
			case VDAT_ERRORATE:
			case VDAT_PRIORITY:
			case VDAT_DELAY:
			case VDAT_TTR:
			    break;

			default: 	/* IS 8073 says to ignore it on CRs */
			    SLOG (tsap_log, LLOG_EXCEPTIONS, NULLCP,
			       ("unknown option 0x%x (length 0x%x) in %s TPDU",
				code, len,
				TPDU_CODE (t) == TPDU_CR ? "CR" : "CC"));
			    if (TPDU_CODE (t) == TPDU_CR)
				break;
			    return DR_PROTOCOL;
		    }
		}
	    }
	    break;

	case TPDU_DR:
	    if (t -> t_li < TPDU_MINLEN (t, DR))
		return DR_LENGTH;
	    if (readx (fd, (char *) &t -> t_dr, DR_SIZE (t), readfnx)
		    != DR_SIZE (t))
		return DR_NETWORK;

	    if (vlen = t -> t_vlen = t -> t_li - TPDU_MINLEN (t, DR)) {
		if ((vptr = t -> t_vdata = malloc ((unsigned) vlen)) == NULL)
		    return DR_CONGEST;
		if (readx (fd, t -> t_vdata, t -> t_vlen, readfnx)
			!= t -> t_vlen)
		    return DR_NETWORK;
		for (; vlen > 0; vptr += len, vlen -= len) {
		    if (vlen < 2)
			return DR_LENGTH;
		    code = *vptr++ & 0xff;
		    len = *vptr++ & 0xff;
		    if ((vlen -= 2) < len)
			return DR_LENGTH;

		    switch (code) {
			case VDAT_ADDITIONAL:
			case VDAT_CHECKSUM:
			    break;

			default: 
			    return DR_PROTOCOL;
		    }
		}
	    }
	    break;

	case TPDU_DT:
	    if (t -> t_li < TPDU_MINLEN (t, DT))
		return DR_LENGTH;
	    if (readx (fd, (char *) &t -> t_dt, DT_SIZE (t), readfnx)
		    != DT_SIZE (t))
		return DR_NETWORK;

	    if (vlen = t -> t_vlen = t -> t_li - TPDU_MINLEN (t, DT)) {
		if ((vptr = t -> t_vdata = malloc ((unsigned) vlen)) == NULL)
		    return DR_CONGEST;
		if (readx (fd, t -> t_vdata, t -> t_vlen, readfnx)
			!= t -> t_vlen)
		    return DR_NETWORK;
		for (; vlen > 0; vptr += len, vlen -= len) {
		    if (vlen < 2)
			return DR_LENGTH;
		    code = *vptr++ & 0xff;
		    len = *vptr++ & 0xff;
		    if ((vlen -= 2) < len)
			return DR_LENGTH;

		    switch (code) {
			case VDAT_CHECKSUM:
			    break;

			default: 
			    return DR_PROTOCOL;
		    }
		}
	    }
	    break;

	case TPDU_ED: 
	    if (t -> t_li < TPDU_MINLEN (t, ED))
		return DR_LENGTH;
	    if (readx (fd, (char *) &t -> t_ed, ED_SIZE (t), readfnx)
		    != ED_SIZE (t))
		return DR_NETWORK;
	    t -> t_ed.ed_nr = ntohs (t -> t_ed.ed_nr);

	    if (vlen = t -> t_vlen = t -> t_li - TPDU_MINLEN (t, ED)) {
		if ((vptr = t -> t_vdata = malloc ((unsigned) vlen)) == NULL)
		    return DR_CONGEST;
		if (readx (fd, t -> t_vdata, t -> t_vlen, readfnx)
			!= t -> t_vlen)
		    return DR_NETWORK;

		for (; vlen > 0; vptr += len, vlen -= len) {
		    if (vlen < 2)
			return DR_LENGTH;
		    code = *vptr++ & 0xff;
		    len = *vptr++ & 0xff;
		    if ((vlen -= 2) < len)
			return DR_LENGTH;

		    switch (code) {
			case VDAT_CHECKSUM:
			case VDAT_SUBSEQ:
			case VDAT_FLOWCTL:
			    break;

			default: 
			    return DR_PROTOCOL;
		    }
		}
	    }
	    break;

	case TPDU_ER:
	    if (t -> t_li < TPDU_MINLEN (t, ER))
		return DR_LENGTH;
	    if (readx (fd, (char *) &t -> t_er, ER_SIZE (t), readfnx)
		    != ER_SIZE (t))
		return DR_NETWORK;

	    if (vlen = t -> t_vlen = t -> t_li - TPDU_MINLEN (t, ER)) {
		if ((vptr = t -> t_vdata = malloc ((unsigned) vlen)) == NULL)
		    return DR_CONGEST;
		if (readx (fd, t -> t_vdata, t -> t_vlen, readfnx)
			!= t -> t_vlen)
		    return DR_NETWORK;
		for (; vlen > 0; vptr += len, vlen -= len) {
		    if (vlen < 2)
			return DR_LENGTH;
		    code = *vptr++ & 0xff;
		    len = *vptr++ & 0xff;
		    if ((vlen -= 2) < len)
			return DR_LENGTH;

		    switch (code) {
			case VDAT_INVALID:
			case VDAT_CHECKSUM:
			    break;

			default: 
			    return DR_PROTOCOL;
		    }
		}
	    }
	    break;

	default: 
	    return DR_PROTOCOL;
    }

    if (len = TPDU_USRLEN (t)) {
	if ((t -> t_qbuf = (struct qbuf *)
			    malloc (sizeof *t -> t_qbuf + (unsigned) len))
		== NULL)
	    return DR_CONGEST;
	t -> t_qbuf -> qb_forw = t -> t_qbuf -> qb_back = t -> t_qbuf;
	if (readx (fd, t -> t_qbuf -> qb_data = t -> t_qbuf -> qb_base,
		t -> t_qbuf -> qb_len = len, readfnx) != len)
	    return DR_NETWORK;
    }

    return OK;
}

/*  */

static int  readx (fd, buffer, n, readfnx)
int	fd;
char    *buffer;
int	n;
IFP	readfnx;
{
    register int    i,
                    cc;
    register char   *bp;

    for (bp = buffer, i = n; i > 0; bp += cc, i -= cc) {
	switch (cc = (*readfnx) (fd, bp, i)) {
	    case NOTOK: 
		return (i = bp - buffer) ? i : NOTOK;

	    case OK: 
		break;

	    default: 
		continue;
	}
	break;
    }

    return (bp - buffer);
}

/*  */

int	tpkt2fd (tb, t, writefnx)
register struct tsapblk *tb;
register struct tsapkt *t;
IFP	writefnx;
{
    int     i,
	    ilen,
            ulen;
    char   *cp,
	   *vptr,
           *outptr;
    register struct udvec  *uv;
    SFP	    pstat;
    SBV	    smask;

    if (t -> t_errno != OK)
	return t -> t_errno;

    if (t -> t_vrsn != TPKT_VRSN)
	if (t -> t_vrsn)
	    return DR_PROTOCOL;
	else
	    t -> t_vrsn = TPKT_VRSN;

    if (t -> t_vdata != NULL) {
	free (t -> t_vdata);
	t -> t_vdata = NULL;
    }
    t -> t_vlen = 0;

    for (ulen = 0, uv = t -> t_udvec; uv -> uv_base; uv++)
	ulen += uv -> uv_len;

    switch (TPDU_CODE (t)) {
	case TPDU_CR:
	case TPDU_CC:
	    if ((vptr = t -> t_vdata =
		    malloc ((unsigned) (3 + 7 + (2 + t -> t_callinglen)
				 + (2 + t -> t_calledlen) + 3))) == NULL)
		return DR_CONGEST;
	    if (t -> t_options) {
		*vptr++ = VDAT_OPTIONS;
		*vptr++ = 1;
		*vptr++ = t -> t_options;
		t -> t_vlen += 3;
	    }
	    if (CR_CLASS (t) != CR_CLASS_TP0 && t -> t_cr.cr_alternate) {
		/* XXX: this doesn't preserve the order of alternates */
		*vptr++ = VDAT_ALTERNATE;
		cp = vptr++;
		if (t -> t_cr.cr_alternate & ALT_TP0)
		    *vptr++ = CR_CLASS_TP0;
		if (t -> t_cr.cr_alternate & ALT_TP1)
		    *vptr++ = CR_CLASS_TP1;
		if (t -> t_cr.cr_alternate & ALT_TP2)
		    *vptr++ = CR_CLASS_TP2;
		if (t -> t_cr.cr_alternate & ALT_TP3)
		    *vptr++ = CR_CLASS_TP3;
		if (t -> t_cr.cr_alternate & ALT_TP4)
		    *vptr++ = CR_CLASS_TP4;
		i = (vptr - cp) - 1;
		*cp = i & 0xff;
		t -> t_vlen += (2 + i) & 0xff;
	    }
	    if (t -> t_callinglen > 0) {
		*vptr++ = VDAT_TSAP_CLI;
		*vptr++ = t -> t_callinglen;
		bcopy (t -> t_calling, vptr, t -> t_callinglen);
		vptr += t -> t_callinglen;
		t -> t_vlen += 2 + t -> t_callinglen;
	    }
	    if (t -> t_calledlen > 0) {
		*vptr++ = VDAT_TSAP_SRV;
		*vptr++ = t -> t_calledlen;
		bcopy (t -> t_called, vptr, t -> t_calledlen);
		vptr += t -> t_calledlen;
		t -> t_vlen += 2 + t -> t_calledlen;
	    }
	    if (t -> t_tpdusize) {
		*vptr++ = VDAT_SIZE;
		*vptr++ = 1;
		*vptr++ = t -> t_tpdusize;
		t -> t_vlen += 3;
	    }
	    if (t -> t_vlen == 0) {
		free (t -> t_vdata);
		t -> t_vdata = NULL;
	    }
	    t -> t_li = TPDU_MINLEN (t, CR) + t -> t_vlen;
	    outptr = (char *) &t -> t_cr;
	    ilen = CR_SIZE (t);
	    break;

	case TPDU_DR: 
	    t -> t_li = TPDU_MINLEN (t, DR) + t -> t_vlen;
	    outptr = (char *) &t -> t_dr;
	    ilen = DR_SIZE (t);
	    break;

	case TPDU_DT: 
	    t -> t_li = TPDU_MINLEN (t, DT) + t -> t_vlen;
	    outptr = (char *) &t -> t_dt;
	    ilen = DT_SIZE (t);
	    break;

	case TPDU_ED: 
	    t -> t_li = TPDU_MINLEN (t, ED) + t -> t_vlen;
	    t -> t_ed.ed_nr = htons (t -> t_ed.ed_nr);
	    outptr = (char *) &t -> t_ed;
	    ilen = ED_SIZE (t);
	    break;

	case TPDU_ER: 
	    t -> t_li = TPDU_MINLEN (t, ER) + t -> t_vlen;
	    outptr = (char *) &t -> t_er;
	    ilen = ER_SIZE (t);
	    if (ulen > 0)
		return DR_PROTOCOL;
	    break;

	default: 
	    return DR_PROTOCOL;
    }

    t -> t_length = htons (t -> t_li + 5 + ulen);

#ifdef	DEBUG
    if (tsap_log -> ll_events & LLOG_PDUS)
	tpkt2text (tsap_log, t, 0);
#endif

    pstat = signal (SIGPIPE, SIG_IGN);
    smask = sigioblock ();

    i = (*writefnx) (tb, t, outptr, ilen);

    (void) sigiomask (smask);
    (void) signal (SIGPIPE, pstat);

    if (i != NOTOK)
	i = OK;
    else
	if (t -> t_errno == DR_UNKNOWN)
	    t -> t_errno = DR_NETWORK;

    return i;
}

/*  */

struct tsapkt *newtpkt (code)
int	code;
{
    register struct tsapkt *t;

    t = (struct tsapkt *) calloc (1, sizeof *t);
    if (t == NULL)
	return NULL;

    t -> t_vrsn = TPKT_VRSN;
    t -> t_code = code;

    return t;
}


int	freetpkt (t)
register struct tsapkt *t;
{
    if (t == NULL)
	return;

    if (t -> t_vdata)
	free (t -> t_vdata);

    if (t -> t_qbuf)
	free ((char *) t -> t_qbuf);

    free ((char *) t);
}

/* ssaplose.c - SPM: you lose */

#ifndef	lint
static char *rcsid = "$Header: /f/osi/ssap/RCS/ssaplose.c,v 7.2 91/02/22 09:45:50 mrose Interim $";
#endif

/* 
 * $Header: /f/osi/ssap/RCS/ssaplose.c,v 7.2 91/02/22 09:45:50 mrose Interim $
 *
 *
 * $Log:	ssaplose.c,v $
 * Revision 7.2  91/02/22  09:45:50  mrose
 * Interim 6.8
 * 
 * Revision 7.1  90/11/21  11:31:43  mrose
 * sun
 * 
 * Revision 7.0  89/11/23  22:25:29  mrose
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
#include <varargs.h>
#include "spkt.h"
#include "tailor.h"

/*  */

#ifndef	lint
int	spktlose (va_alist)
va_dcl
{
    int	    reason,
    	    result,
    	    sd,
    	    secs,
	    value;
    register struct ssapblk *sb;
    register struct ssapkt *s;
    struct SSAPindication   sis;
    register struct SSAPindication *si;
    register struct SSAPabort *sa;
    struct TSAPdata txs;
    register struct TSAPdata   *tx = &txs;
    struct TSAPdisconnect   tds;
    register struct TSAPdisconnect *td = &tds;
    va_list ap;

    va_start (ap);

    sd = va_arg (ap, int);

    si = va_arg (ap, struct SSAPindication *);
    if (si == NULL)
	si = &sis;

    reason = va_arg (ap, int);
    value = reason & SC_REFUSE;
    reason &= ~SC_REFUSE;

    result = _ssaplose (si, reason, ap);

    va_end (ap);

    if ((sa = &si -> si_abort) -> sa_cc > 0) {
	SLOG (ssap_log, LLOG_EXCEPTIONS, NULLCP,
	      ("spktlose [%s] %*.*s", SErrString (sa -> sa_reason),
	       sa -> sa_cc, sa -> sa_cc, sa -> sa_data));
    }
    else
	SLOG (ssap_log, LLOG_EXCEPTIONS, NULLCP,
	      ("spktlose [%s]", SErrString (sa -> sa_reason)));

    if (value && SC_OFFICIAL (reason)) {
	if ((s = newspkt (SPDU_RF)) == NULL)
	    return result;

	s -> s_mask |= SMASK_RF_DISC;
	s -> s_rf_disconnect |= RF_DISC_RELEASE;

	if (reason == SC_VERSION) {
	    s -> s_mask |= SMASK_RF_VRSN;
	    s -> s_rf_version = SB_ALLVRSNS;
	}

	if (s -> s_rdata = malloc ((unsigned) (s -> s_rlen = 1)))
	    *s -> s_rdata = reason & 0xff;

	secs = ses_rf_timer;
    }
    else {
	if ((s = newspkt (SPDU_AB)) == NULL)
	    return result;

	s -> s_mask |= SMASK_SPDU_AB | SMASK_AB_DISC;
	s -> s_ab_disconnect = AB_DISC_RELEASE;
	switch (reason) {
	    case SC_PROTOCOL: 
	    case SC_VERSION: 
		s -> s_ab_disconnect |= AB_DISC_PROTO;
		break;

	    case SC_CONGEST: 
	    case SC_SSAPID: 
		break;

	    default: 
		s -> s_ab_disconnect |= AB_DISC_UNKNOWN;
		break;
	}

	secs = ses_ab_timer;
    }

    value = spkt2sd (s, sd,
		(sb = findsblk (sd)) && (sb -> sb_flags & SB_EXPD), si);

    freespkt (s);
    if (value == NOTOK)
	return result;

    if (secs >= 0)
	switch (TReadRequest (sd, tx, secs, td)) {
	    case OK:
	    default:
		TXFREE (tx);
		break;

	    case NOTOK:
		break;
	}

    return result;
}
#else
/* VARARGS5 */

int	spktlose (sd, si, reason, what, fmt)
int	sd,
	reason;
struct SSAPindication *si;
char   *what,
       *fmt;
{
    return spktlose (sd, si, reason, what, fmt);
}
#endif

/*  */

#ifndef	lint
int	ssaplose (va_alist)
va_dcl
{
    int	    reason,
    	    result;
    struct SSAPindication *si;
    va_list ap;

    va_start (ap);

    si = va_arg (ap, struct SSAPindication *);
    reason = va_arg (ap, int);

    result = _ssaplose (si, reason, ap);

    va_end (ap);

    return result;
}
#else
/* VARARGS4 */

int	ssaplose (si, reason, what, fmt)
struct SSAPindication *si;
int	reason;
char   *what,
       *fmt;
{
    return ssaplose (si, reason, what, fmt);
}
#endif

/*  */

#ifndef	lint
static int  _ssaplose (si, reason, ap)	/* what, fmt, args ... */
register struct SSAPindication *si;
int	reason;
va_list	ap;
{
    register char  *bp;
    char    buffer[BUFSIZ];
    register struct SSAPabort *sa;

    if (si) {
	bzero ((char *) si, sizeof *si);
	si -> si_type = SI_ABORT;
	sa = &si -> si_abort;

	asprintf (bp = buffer, ap);
	bp += strlen (bp);

	sa -> sa_peer = 0;
	sa -> sa_reason = reason;
	copySSAPdata (buffer, bp - buffer, sa);
    }

    return NOTOK;
}
#endif

/* rtsaplose.c - RTPM: you lose */

#ifndef	lint
static char *rcsid = "$Header: /f/osi/rtsap/RCS/rtsaplose.c,v 7.1 91/02/22 09:42:40 mrose Interim $";
#endif

/* 
 * $Header: /f/osi/rtsap/RCS/rtsaplose.c,v 7.1 91/02/22 09:42:40 mrose Interim $
 *
 *
 * $Log:	rtsaplose.c,v $
 * Revision 7.1  91/02/22  09:42:40  mrose
 * Interim 6.8
 * 
 * Revision 6.0  89/03/18  23:43:29  mrose
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
#include <varargs.h>
#include "rtpkt.h"
#include "tailor.h"

/*  */

#ifndef	lint
int	rtpktlose (va_alist)
va_dcl
{
    int	    reason,
    	    result,
    	    value;
    register struct assocblk *acb;
    register struct RtSAPindication *rti;
    register struct RtSAPabort *rta;
    va_list ap;

    va_start (ap);

    acb = va_arg (ap, struct assocblk *);
    rti = va_arg (ap, struct RtSAPindication *);
    reason = va_arg (ap, int);
    
    result = _rtsaplose (rti, reason, ap);

    va_end (ap);

    if ((rta = &rti -> rti_abort) -> rta_cc > 0) {
	SLOG (rtsap_log, LLOG_EXCEPTIONS, NULLCP,
	      ("rtpktlose [%s] %*.*s", RtErrString (rta -> rta_reason),
	       rta -> rta_cc, rta -> rta_cc, rta -> rta_data));
    }
    else
	SLOG (rtsap_log, LLOG_EXCEPTIONS, NULLCP,
	      ("rtpktlose [%s]", RtErrString (rta -> rta_reason)));

    if (acb == NULLACB
	    || acb -> acb_fd == NOTOK
	    || acb -> acb_rtpktlose == NULLIFP)
	return result;

    switch (reason) {
	case RTS_PROTOCOL: 
	    value = ABORT_PROTO;
	    break;

	case RTS_CONGEST: 
	    value = ABORT_TMP;
	    break;

	default: 
	    value = ABORT_LSP;
	    break;
    }

    (*acb -> acb_rtpktlose) (acb, value);

    return result;
}
#else
/* VARARGS5 */

int	rtpktlose (acb, rti, reason, what, fmt)
struct assocblk *acb;
struct RtSAPindication *rti;
int     reason;
char   *what,
       *fmt;
{
    return rtpktlose (acb, rti, reason, what, fmt);
}
#endif

/*  */

#ifndef	lint
int	rtsaplose (va_alist)
va_dcl
{
    int	    reason,
    	    result;
    struct RtSAPindication *rti;
    va_list ap;

    va_start (ap);

    rti = va_arg (ap, struct RtSAPindication *);
    reason = va_arg (ap, int);

    result = _rtsaplose (rti, reason, ap);

    va_end (ap);

    return result;
}
#else
/* VARARGS4 */

int	rtsaplose (rti, reason, what, fmt)
struct RtSAPindication *rti;
int     reason;
char   *what,
       *fmt;
{
    return rtsaplose (rti, reason, what, fmt);
}
#endif

/*  */

#ifndef	lint
static int  _rtsaplose (rti, reason, ap)  /* what, fmt, args ... */
register struct RtSAPindication *rti;
int     reason;
va_list	ap;
{
    register char  *bp;
    char    buffer[BUFSIZ];
    register struct RtSAPabort *rta;

    if (rti) {
	bzero ((char *) rti, sizeof *rti);
	rti -> rti_type = RTI_ABORT;
	rta = &rti -> rti_abort;

	asprintf (bp = buffer, ap);
	bp += strlen (bp);

	rta -> rta_peer = 0;
	rta -> rta_reason = reason;
	copyRtSAPdata (buffer, bp - buffer, rta);
    }

    return NOTOK;
}
#endif

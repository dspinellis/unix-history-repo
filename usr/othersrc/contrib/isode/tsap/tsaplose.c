/* tsaplose.c - TPM: you lose */


#ifndef	lint
static char *rcsid = "$Header: /f/osi/tsap/RCS/tsaplose.c,v 7.2 91/02/22 09:47:36 mrose Interim $";
#endif

/* 
 * $Header: /f/osi/tsap/RCS/tsaplose.c,v 7.2 91/02/22 09:47:36 mrose Interim $
 *
 *
 * $Log:	tsaplose.c,v $
 * Revision 7.2  91/02/22  09:47:36  mrose
 * Interim 6.8
 * 
 * Revision 7.1  89/12/07  22:15:50  mrose
 * touch-up
 * 
 * Revision 7.0  89/11/23  22:30:51  mrose
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
#include "tpkt.h"
#include "mpkt.h"
#include "tailor.h"


#ifdef	LPP
#undef	MGMT
#endif

/*  */

#ifndef	lint
int	tpktlose (va_alist)
va_dcl
{
    int	    reason,
    	    result;
    register struct tsapblk *tb;
    struct TSAPdisconnect   tds;
    register struct TSAPdisconnect  *td;
    va_list ap;

    va_start (ap);

    tb = va_arg (ap, struct tsapblk *);

    td = va_arg (ap, struct TSAPdisconnect *);
    if (td == NULL)
	td = &tds;

    reason = va_arg (ap, int);

    result = _tsaplose (td, reason, ap);

    va_end (ap);

    if (td -> td_cc > 0) {
	SLOG (tsap_log, LLOG_EXCEPTIONS, NULLCP,
	  ("tpktlose [%s] %*.*s", TErrString (td -> td_reason), td -> td_cc,
	   td -> td_cc, td -> td_data));
    }
    else
	SLOG (tsap_log, LLOG_EXCEPTIONS, NULLCP,
	      ("tpktlose [%s]", TErrString (td -> td_reason)));

#ifdef  MGMT
    if (tb -> tb_manfnx)
	switch (reason) {
	    case DR_REMOTE:
	    case DR_CONGEST:
	        (*tb -> tb_manfnx) (CONGEST, tb);
		break;

	    case DR_PROTOCOL:
	    case DR_MISMATCH:
		(*tb -> tb_manfnx) (PROTERR, tb);
		break;

	    case DR_SESSION:
	    case DR_ADDRESS:
	    case DR_CONNECT:
	    case DR_DUPLICATE:
	    case DR_OVERFLOW:
	    case DR_REFUSED:
		(*tb -> tb_manfnx) (CONFIGBAD, tb);
		break;

	    default:
		(*tb -> tb_manfnx) (OPREQINBAD, tb);
	}
#endif

    (*tb -> tb_losePfnx) (tb, reason, td);

    return result;
}
#else
/* VARARGS5 */

int	tpktlose (tb, td, reason, what, fmt)
struct tsapblk *tb;
struct TSAPdisconnect *td;
int	reason;
char   *what,
       *fmt;
{
    return tpktlose (tb, td, reason, what, fmt);
}
#endif

/*  */

#ifndef	lint
int	tsaplose (va_alist)
va_dcl
{
    int	    reason,
    	    result;
    struct TSAPdisconnect *td;
    va_list ap;

    va_start (ap);

    td = va_arg (ap, struct TSAPdisconnect *);
    reason = va_arg (ap, int);

    result = _tsaplose (td, reason, ap);

    va_end (ap);

    return result;

}
#else
/* VARARGS4 */

int	tsaplose (td, reason, what, fmt)
struct TSAPdisconnect *td;
int	reason;
char   *what,
       *fmt;
{
    return tsaplose (td, reason, what, fmt);
}
#endif

/*  */

#ifndef	lint
static int  _tsaplose (td, reason, ap)	/* what, fmt, args ... */
register struct TSAPdisconnect *td;
int     reason;
va_list	ap;
{
    register char  *bp;
    char    buffer[BUFSIZ];

    if (td) {
	bzero ((char *) td, sizeof *td);

	asprintf (bp = buffer, ap);
	bp += strlen (bp);

	td -> td_reason = reason;
	copyTSAPdata (buffer, bp - buffer, td);
    }

    return NOTOK;
}
#endif

/* daplose.c - DAP: Support for directory protocol mappings */

#ifndef	lint
static char *rcsid = "$Header: /f/osi/dsap/net/RCS/daplose.c,v 7.1 91/02/22 09:21:01 mrose Interim $";
#endif

/* 
 * $Header: /f/osi/dsap/net/RCS/daplose.c,v 7.1 91/02/22 09:21:01 mrose Interim $
 *
 *
 * $Log:	daplose.c,v $
 * Revision 7.1  91/02/22  09:21:01  mrose
 * Interim 6.8
 * 
 * Revision 7.0  90/07/26  14:45:22  mrose
 * *** empty log message ***
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
#include "tailor.h"
#include "quipu/dap2.h"

/*  */

#ifndef	lint
int	daplose (va_alist)
va_dcl
{
    int	    reason,
	    result;
    struct DAPindication *di;
    va_list ap;

    va_start (ap);

    di = va_arg (ap, struct DAPindication *);
    reason = va_arg (ap, int);

    result = _daplose (di, reason, ap);

    va_end (ap);

    return result;
}
#else
/* VARARGS4 */

int	daplose (di, reason, what, fmt)
struct DAPindication *di;
int	reason;
char   *what,
       *fmt;
{
    return daplose (di, reason, what, fmt);
}
#endif

/*  */

#ifndef	lint
static int  _daplose (di, reason, ap)  /* what, fmt, args ... */
register struct DAPindication *di;
int     reason;
va_list	ap;
{
    register char  *bp;
    char    buffer[BUFSIZ];
    struct DAPabort	* da;

    if (di) {
	bzero ((char *) di, sizeof *di);
	di->di_type = DI_ABORT;
	da = &(di->di_abort);
	da->da_reason = reason;

	asprintf (bp = buffer, ap);
	bp += strlen (bp);

	copyDAPdata (buffer, bp - buffer, da);
    }

    return NOTOK;
}
#endif

#ifndef	lint
int	dapreject (va_alist)
va_dcl
{
    int	    reason,
	    id,
	    result;
    struct DAPindication *di;
    va_list ap;

    va_start (ap);

    di = va_arg (ap, struct DAPindication *);
    reason = va_arg (ap, int);
    id = va_arg (ap, int);

    result = _dapreject (di, reason, id, ap);

    va_end (ap);

    return result;
}
#else
/* VARARGS4 */

int	dapreject (di, reason, id, what, fmt)
struct DAPindication *di;
int	reason;
int	id;
char   *what,
       *fmt;
{
    return dapreject (di, reason, id, what, fmt);
}
#endif

/*  */

#ifndef	lint
static int  _dapreject (di, reason, id, ap)  /* what, fmt, args ... */
register struct DAPindication *di;
int     reason;
int	id;
va_list	ap;
{
    register char  *bp;
    char    buffer[BUFSIZ];
    struct DAPpreject	* dp;

    if (di) {
	bzero ((char *) di, sizeof *di);
	di->di_type = DI_PREJECT;
	dp = &(di->di_preject);
	dp->dp_id = id;
	dp->dp_reason = reason;

	asprintf (bp = buffer, ap);
	bp += strlen (bp);

	copyDAPdata (buffer, bp - buffer, dp);
    }

    return (NOTOK);
}
#endif

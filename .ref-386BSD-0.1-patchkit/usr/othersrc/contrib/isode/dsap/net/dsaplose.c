/* dsaplose.c - DSAP: Support for directory protocol mappings */

#ifndef	lint
static char *rcsid = "$Header: /f/osi/dsap/net/RCS/dsaplose.c,v 7.1 91/02/22 09:21:21 mrose Interim $";
#endif

/* 
 * $Header: /f/osi/dsap/net/RCS/dsaplose.c,v 7.1 91/02/22 09:21:21 mrose Interim $
 *
 *
 * $Log:	dsaplose.c,v $
 * Revision 7.1  91/02/22  09:21:21  mrose
 * Interim 6.8
 * 
 * Revision 7.0  90/07/26  14:45:55  mrose
 * *** empty log message ***
 * 
 */

/*
 *                                NOTICE
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
#include "quipu/dsap.h"

/*  */

#ifndef	lint
int	dsaplose (va_alist)
va_dcl
{
    int	    reason,
	    result;
    struct DSAPindication *di;
    va_list ap;

    va_start (ap);

    di = va_arg (ap, struct DSAPindication *);
    reason = va_arg (ap, int);

    result = _dsaplose (di, reason, ap);

    va_end (ap);

    return result;
}
#else
/* VARARGS4 */

int	dsaplose (di, reason, what, fmt)
struct DSAPindication *di;
int	reason;
char   *what,
       *fmt;
{
    return dsaplose (di, reason, what, fmt);
}
#endif

/*  */

#ifndef	lint
static int  _dsaplose (di, reason, ap)  /* what, fmt, args ... */
register struct DSAPindication *di;
int     reason;
va_list	ap;
{
    register char  *bp;
    char    buffer[BUFSIZ];
    struct DSAPabort	* da;

    if (di) {
	bzero ((char *) di, sizeof *di);
	di->di_type = DI_ABORT;
	da = &(di->di_abort);
	da->da_reason = reason;

	asprintf (bp = buffer, ap);
	bp += strlen (bp);

	copyDSAPdata (buffer, bp - buffer, da);
    }

    return NOTOK;
}
#endif

#ifndef	lint
int	dsapreject (va_alist)
va_dcl
{
    int	    reason,
	    id,
	    result;
    struct DSAPindication *di;
    va_list ap;

    va_start (ap);

    di = va_arg (ap, struct DSAPindication *);
    reason = va_arg (ap, int);
    id = va_arg (ap, int);

    result = _dsapreject (di, reason, id, ap);

    va_end (ap);

    return result;
}
#else
/* VARARGS4 */

int	dsapreject (di, reason, id, what, fmt)
struct DSAPindication *di;
int	reason;
int	id;
char   *what,
       *fmt;
{
    return dsapreject (di, reason, id, what, fmt);
}
#endif

/*  */

#ifndef	lint
static int  _dsapreject (di, reason, id, ap)  /* what, fmt, args ... */
register struct DSAPindication *di;
int     reason;
int	id;
va_list	ap;
{
    register char  *bp;
    char    buffer[BUFSIZ];
    struct DSAPpreject	* dp;

    if (di) {
	bzero ((char *) di, sizeof *di);
	di->di_type = DI_PREJECT;
	dp = &(di->di_preject);
	dp->dp_id = id;
	dp->dp_reason = reason;

	asprintf (bp = buffer, ap);
	bp += strlen (bp);

	copyDSAPdata (buffer, bp - buffer, dp);
    }

    return (NOTOK);
}
#endif

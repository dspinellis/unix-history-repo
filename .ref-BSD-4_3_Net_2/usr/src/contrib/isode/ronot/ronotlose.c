/* ronotlose.c - RONOT: Support for ABSTRACT-BIND mappings */

#ifndef	lint
static char *rcsid = "$Header: /f/osi/ronot/RCS/ronotlose.c,v 7.2 91/02/22 09:50:33 mrose Interim $";
#endif

/* 
 * $Header: /f/osi/ronot/RCS/ronotlose.c,v 7.2 91/02/22 09:50:33 mrose Interim $
 *
 *
 * $Log:	ronotlose.c,v $
 * Revision 7.2  91/02/22  09:50:33  mrose
 * Interim 6.8
 * 
 * Revision 7.1  90/07/26  14:33:59  mrose
 * template
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
#include "logger.h"
#include "ronot.h"

/*  */

#ifndef	lint
int	ronotlose (va_alist)
va_dcl
{
    int	    reason,
	    result;
    struct RoNOTindication *rni;
    va_list ap;

    va_start (ap);

    rni = va_arg (ap, struct RoNOTindication *);
    reason = va_arg (ap, int);

    result = _ronotlose (rni, reason, ap);

    va_end (ap);

    return result;
}
#else
/* VARARGS4 */

int	ronotlose (rni, reason, what, fmt)
struct RoNOTindication *rni;
int	reason;
char   *what,
       *fmt;
{
    return ronotlose (rni, reason, what, fmt);
}
#endif

/*  */

#ifndef	lint
static int  _ronotlose (rni, reason, ap)  /* what, fmt, args ... */
register struct RoNOTindication *rni;
int     reason;
va_list	ap;
{
    register char  *bp;
    char    buffer[BUFSIZ];

    if (rni) {
	bzero ((char *) rni, sizeof *rni);
	rni -> rni_reason = reason;

	asprintf (bp = buffer, ap);
	bp += strlen (bp);

	copyRoNOTdata (buffer, bp - buffer, rni);
    }

    return NOTOK;
}
#endif

/*    ACSAP interface */

int	  acs2ronotlose (rni, event, aca)
struct RoNOTindication	* rni;
char			* event;
struct AcSAPabort	* aca;
{

	char	* cp;
	char	  buffer[BUFSIZ];

/*
* ADT: Not sure about having events logged from the BIND abstraction,
* since it is a MACRO defined abstraction and not a level or ASE.
* Currently not performed but would be something like the following:
*/
/*
	if (event)
		SLOG (addr_log, LLOG_EXCEPTIONS, NULLCP,
			((aca->aca_cc > 0) ? "%s: %s [%*.*s]" : "%s: %s",
			event, AcErrString (aca->aca_reason), aca->aca_cc,
			aca->aca_cc, aca->aca_data));
*/

/* event string used as (pseudo-)events label of the BIND abstraction */

	if (event)
		(void) sprintf (cp = buffer, " {%s} (%s at ACSE)",
			event, AcErrString (aca->aca_reason));
	else
		(void) sprintf (cp = buffer, " (%s at ACSE)",
			AcErrString (aca->aca_reason));

	if (aca->aca_cc > 0)
		return (ronotlose (rni, RBI_ACSE, NULLCP, "%*.*s%s",
			aca->aca_cc, aca->aca_cc, aca->aca_data, cp));
	else
		return (ronotlose (rni, RBI_ACSE, NULLCP, "%s", cp));
}


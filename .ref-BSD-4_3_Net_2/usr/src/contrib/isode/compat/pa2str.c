/* pa2str.c - pretty-print PSAPaddr */

#ifndef	lint
static char *rcsid = "$Header: /f/osi/compat/RCS/pa2str.c,v 7.2 91/02/22 09:15:40 mrose Interim $";
#endif

/* 
 * $Header: /f/osi/compat/RCS/pa2str.c,v 7.2 91/02/22 09:15:40 mrose Interim $
 *
 *
 * $Log:	pa2str.c,v $
 * Revision 7.2  91/02/22  09:15:40  mrose
 * Interim 6.8
 * 
 * Revision 7.1  90/11/21  11:29:53  mrose
 * sun
 * 
 * Revision 7.0  89/11/30  20:55:02  mrose
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

#include <ctype.h>
#include <stdio.h>
#include "general.h"
#include "manifest.h"
#include "isoaddrs.h"

/*    Presentation Address to String */

char   *pa2str (px)
register struct PSAPaddr *px;
{
    register char *bp;
    struct PSAPaddr pas;
    register struct PSAPaddr *pa = &pas;
    register struct TSAPaddr *ta = &pa -> pa_addr.sa_addr;
    static char buffer[BUFSIZ];

    bp = buffer;

    *pa = *px;	/* struct copy */
    if (ta -> ta_selectlen > 0
	    && ta -> ta_selectlen < sizeof ta -> ta_selector) {
	register char *dp,
		      *ep;
	register struct TSAPaddr *tz;

	for (ep = (dp = ta -> ta_selector) + ta -> ta_selectlen, *ep = NULL;
	         dp < ep;
	         dp++)
	    if (!isprint ((u_char) *dp) && *dp != ' ')
		break;
	if (dp >= ep && (tz = str2taddr (ta -> ta_selector))) {
	    pa -> pa_addr.sa_addr = *tz;	    /* struct copy */
	    (void) sprintf (bp, "%s through TS bridge at ",
			    paddr2str (pa, NULLNA));
	    bp += strlen (bp);

	    bzero ((char *) pa, sizeof *pa);
	    *ta = px -> pa_addr.sa_addr;    /* struct copy */
	    ta -> ta_selectlen = 0;
	}
    }
    (void) strcpy (bp, paddr2str (pa, NULLNA));

    return buffer;
}

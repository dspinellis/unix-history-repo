/* ssaperror.c - return SSAP error code in string form */

#ifndef	lint
static char *rcsid = "$Header: /f/osi/ssap/RCS/ssaperror.c,v 7.2 91/02/22 09:45:45 mrose Interim $";
#endif

/* 
 * $Header: /f/osi/ssap/RCS/ssaperror.c,v 7.2 91/02/22 09:45:45 mrose Interim $
 *
 *
 * $Log:	ssaperror.c,v $
 * Revision 7.2  91/02/22  09:45:45  mrose
 * Interim 6.8
 * 
 * Revision 7.1  91/01/11  07:09:17  mrose
 * jpo
 * 
 * Revision 7.0  89/11/23  22:25:25  mrose
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
#include "ssap.h"

/*  */

static char *reject_err0[] = {
    "Reason not specified",
    "Temporary congestion",
    "Rejected"
};

static int reject_err0_cnt = sizeof reject_err0 / sizeof reject_err0[0];


static char *reject_err8[] = {
    "unknown error code 0x80",
    "SSAP identifier unknown",
    "SS-user not attached to SSAP",
    "Congestion at SSAP",
    "Proposed protocol versions not supported",
    "Address unknown",
    "Connect request refused on this network connection", 
    "Transport disconnect",
    "Provider-initiated abort",
    "Protocol error",
    "Invalid parameter",
    "Invalid operation",
    "Timer expired",
    "Indications waiting"
};

static int reject_err8_cnt = sizeof reject_err8 / sizeof reject_err8[0];


/*  */

char   *SErrString(code)
register int	code;
{
    register int    fcode;
    static char buffer[50];

    code &= 0xff;
    if (code & SC_BASE) {
	if ((fcode = code & ~SC_BASE) < reject_err8_cnt)
	    return reject_err8[fcode];
    }
    else
	if (code < reject_err0_cnt)
	    return reject_err0[code];

    (void) sprintf (buffer, "unknown error code 0x%x", code);
    return buffer;
}

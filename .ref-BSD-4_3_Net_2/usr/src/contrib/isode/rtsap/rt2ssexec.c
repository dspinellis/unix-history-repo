/* rt2ssexec.c - RTPM: exec */

#ifndef	lint
static char *rcsid = "$Header: /f/osi/rtsap/RCS/rt2ssexec.c,v 7.2 91/02/22 09:42:29 mrose Interim $";
#endif

/* 
 * $Header: /f/osi/rtsap/RCS/rt2ssexec.c,v 7.2 91/02/22 09:42:29 mrose Interim $
 *
 *
 * $Log:	rt2ssexec.c,v $
 * Revision 7.2  91/02/22  09:42:29  mrose
 * Interim 6.8
 * 
 * Revision 7.1  90/07/01  21:07:08  mrose
 * pepsy
 * 
 * Revision 6.0  89/03/18  23:43:19  mrose
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
#include "isoservent.h"
#include "tailor.h"

/*    SERVER only */

int	RtExec (ss, rti, arg1, arg2, hook, setperms)
struct SSAPstart *ss;
struct RtSAPindication *rti;
char   *arg1,
       *arg2;
IFP	hook,
	setperms;
{
    int     result,
            result2;
    register struct isoservent *is;
    register PE	    pe;
    struct SSAPref ref;
    struct SSAPindication   sis;
    register struct SSAPindication *si = &sis;
    struct type_OACS_PConnect	*pcon = NULL;

    missingP (ss);
    missingP (rti);
    missingP (arg1);
    missingP (arg2);

    /* acsap_conntype = CN_OPEN, acsap_data = NULLPE; */
    if ((pe = ssdu2pe (ss -> ss_data, ss -> ss_cc, NULLCP, &result)) == NULLPE
	    || parse_OACS_PConnect (pe, 1, NULLIP, NULLVP, &pcon) == NOTOK) {
	if (pe)
	    pe_free (pe);
	if (result == PS_ERR_NMEM) {
    congest: ;
	    result = SC_CONGESTION, result2 = RTS_CONGEST;
	}
	else
	    result = SC_REJECTED, result2 = RTS_PROTOCOL;
	goto out;
    }

    PLOGP (rtsap_log,OACS_PConnect, pe, "PConnect", 1);

    pe_free (pe);

    if (pcon -> pUserData -> member_OACS_2 -> offset
	    != type_OACS_ConnectionData_open) {
	result = SC_REJECTED, result2 = RTS_ADDRESS;
	goto out;
    }	
    if (is = getisoserventbyport ("rtsap", 
	    (u_short) htons ((u_short) pcon -> pUserData -> applicationProtocol))) {
	*is -> is_tail++ = arg1;
	*is -> is_tail++ = arg2;
	*is -> is_tail = NULL;
    }
    else {
	result = SC_REJECTED, result2 = RTS_ADDRESS;
	goto out;
    }

    switch (hook ? (*hook) (is, rti) : OK) {
	case NOTOK: 
	    if (pcon)
		free_OACS_PConnect(pcon);
	    return NOTOK;

	case DONE: 
	    if (pcon)
		free_OACS_PConnect(pcon);
	    return OK;

	case OK: 
	    if (setperms)
		(void) (*setperms) (is);
	    (void) execv (*is -> is_vec, is -> is_vec);/* fall */
	    SLOG (rtsap_log, LLOG_FATAL, *is -> is_vec, ("unable to exec"));
	default: 
	    goto congest;
    }

out: ;
    if (pcon)
	free_OACS_PConnect(pcon);
    SSFREE (ss);

    bzero ((char *) &ref, sizeof ref);
    (void) SConnResponse (ss -> ss_sd, &ref, NULLSA, result, 0, 0,
		SERIAL_NONE, NULLCP, 0, si);
    return rtsaplose (rti, result2, NULLCP, NULLCP);
}

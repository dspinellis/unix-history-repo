/* psapselect.c - PPM: map descriptors */

#ifndef	lint
static char *rcsid = "$Header: /f/osi/psap2-lpp/RCS/psapselect.c,v 7.1 91/02/22 09:38:16 mrose Interim $";
#endif

/* 
 * $Header: /f/osi/psap2-lpp/RCS/psapselect.c,v 7.1 91/02/22 09:38:16 mrose Interim $
 *
 * Contributed by The Wollongong Group, Inc.
 *
 *
 * $Log:	psapselect.c,v $
 * Revision 7.1  91/02/22  09:38:16  mrose
 * Interim 6.8
 * 
 * Revision 7.0  89/11/23  22:16:02  mrose
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
#define	LPP
#include "ppkt.h"

/*    map presentation descriptors for select() */

int	PSelectMask (sd, mask, nfds, pi)
int	sd;
fd_set *mask;
int    *nfds;
struct PSAPindication *pi;
{
    SBV     smask;
    int	    reason,
	    result;
    register struct psapblk *pb;

    missingP (mask);
    missingP (nfds);
    missingP (pi);

    smask = sigioblock ();

    if ((pb = findpblk (sd)) == NULL) {
	(void) sigiomask (smask);
	return psaplose (pi, PC_PARAMETER, NULLCP,
			    "invalid presentation descriptor");
    }

    result = pb -> pb_checkfnx ? (*pb -> pb_checkfnx) (pb, pi) : OK;
    if (result == NOTOK && (reason = pi -> pi_abort.pa_reason) != PC_TIMER) {
	if (PC_FATAL (reason))
	    freepblk (pb);
    }
    else {
	FD_SET (pb -> pb_fd, mask);
	if (pb -> pb_fd > *nfds)
	    *nfds = pb -> pb_fd + 1;
    }

    (void) sigiomask (smask);

    return result;
}

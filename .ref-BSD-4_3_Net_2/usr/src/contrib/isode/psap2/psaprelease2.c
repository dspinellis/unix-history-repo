/* psaprelease2.c - PPM: respond to release */

#ifndef	lint
static char *rcsid = "$Header: /f/osi/psap2/RCS/psaprelease2.c,v 7.1 91/02/22 09:37:42 mrose Interim $";
#endif

/* 
 * $Header: /f/osi/psap2/RCS/psaprelease2.c,v 7.1 91/02/22 09:37:42 mrose Interim $
 *
 *
 * $Log:	psaprelease2.c,v $
 * Revision 7.1  91/02/22  09:37:42  mrose
 * Interim 6.8
 * 
 * Revision 7.0  89/11/23  22:14:26  mrose
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
#include "ppkt.h"

/*    P-RELEASE.RESPONSE */

int	PRelResponse (sd, status, data, ndata, pi)
int	sd;
int	status;
PE     *data;
int	ndata;
struct PSAPindication *pi;
{
    SBV	    smask;
    int	    len,
	    result;
    char   *base,
	   *realbase;
    register struct psapblk *pb;
    struct SSAPindication   sis;
    register struct SSAPabort  *sa = &sis.si_abort;

    toomuchP (data, ndata, NPDATA, "release");
    missingP (pi);

    smask = sigioblock ();

    psapFsig (pb, sd);

    switch (result = info2ssdu (pb, pi, data, ndata, &realbase, &base, &len,
				"P-RELEASE user-data", PPDU_NONE)) {
        case NOTOK:
	    goto out2;

	case OK:
	default:
	    break;

	case DONE:
	    result = NOTOK;
	    goto out1;
    }

    if ((result = SRelResponse (pb -> pb_fd, status, base, len, &sis))
	    == NOTOK)
	if (SC_FATAL (sa -> sa_reason)) {
	    (void) ss2pslose (pb, pi, "SRelResponse", sa);
	    goto out2;
	}
	else {
	    (void) ss2pslose (NULLPB, pi, "SRelResponse", sa);
	    goto out1;
	}

    if (status == SC_ACCEPT)
	pb -> pb_fd = NOTOK;
    else
	pb -> pb_flags &= ~PB_FINN;

    result = OK;

out2: ;
    if (result == NOTOK || status == SC_ACCEPT)
	freepblk (pb);
out1: ;
    if (base)
	free (base);

    (void) sigiomask (smask);

    return result;
}

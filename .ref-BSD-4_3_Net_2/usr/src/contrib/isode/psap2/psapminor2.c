/* psapminor2.c - PPM: respond to minorsyncs */

#ifndef	lint
static char *rcsid = "$Header: /f/osi/psap2/RCS/psapminor2.c,v 7.1 91/02/22 09:37:38 mrose Interim $";
#endif

/* 
 * $Header: /f/osi/psap2/RCS/psapminor2.c,v 7.1 91/02/22 09:37:38 mrose Interim $
 *
 *
 * $Log:	psapminor2.c,v $
 * Revision 7.1  91/02/22  09:37:38  mrose
 * Interim 6.8
 * 
 * Revision 7.0  89/11/23  22:14:25  mrose
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

/*    P-MINOR-SYNC.RESPONSE */

int	PMinSyncResponse (sd, ssn, data, ndata, pi)
int	sd;
long	ssn;
int	ndata;
PE     *data;
struct PSAPindication *pi;
{
    SBV	    smask;
    int     len,
	    result;
    char   *base,
	   *realbase;
    register struct psapblk *pb;
    struct SSAPindication   sis;
    register struct SSAPabort  *sa = &sis.si_abort;

    toomuchP (data, ndata, NPDATA, "minorsync");
    missingP (pi);

    smask = sigioblock ();

    psapPsig (pb, sd);

    if ((result = info2ssdu (pb, pi, data, ndata, &realbase, &base, &len,
			     "P-MINOR-SYNC user-data", PPDU_NONE)) != OK)
	goto out2;

    if ((result = SMinSyncResponse (sd, ssn, base, len, &sis)) == NOTOK)
	if (SC_FATAL (sa -> sa_reason))
	    (void) ss2pslose (pb, pi, "SMinSyncResponse", sa);
	else {
	    (void) ss2pslose (NULLPB, pi, "SMinSyncResponse", sa);
	    goto out1;
	}

out2: ;
    if (result == NOTOK)
	freepblk (pb);
    else
	if (result == DONE)
	    result = NOTOK;
out1: ;
    if (realbase)
	free (realbase);
    else
	if (base)
	    free (base);

    (void) sigiomask (smask);

    return result;
}

/* psapresync1.c - PPM: initiate resyncs */

#ifndef	lint
static char *rcsid = "$Header: /f/osi/psap2/RCS/psapresync1.c,v 7.1 91/02/22 09:37:47 mrose Interim $";
#endif

/* 
 * $Header: /f/osi/psap2/RCS/psapresync1.c,v 7.1 91/02/22 09:37:47 mrose Interim $
 *
 *
 * $Log:	psapresync1.c,v $
 * Revision 7.1  91/02/22  09:37:47  mrose
 * Interim 6.8
 * 
 * Revision 7.0  89/11/23  22:14:31  mrose
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

/*    P-RESYNCHRONIZE.REQUEST */

int	PReSyncRequest (sd, type, ssn, settings, data, ndata, pi)
int	sd;
int	type,
	settings,
	ndata;
long	ssn;
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

    toomuchP (data, ndata, NPDATA, "resync");
    missingP (pi);

    smask = sigioblock ();

    psapPsig (pb, sd);

    if ((result = info2ssdu (pb, pi, data, ndata, &realbase, &base, &len,
			     "P-RESYNCHRONIZE user-data", PPDU_RS)) != OK)
	goto out2;

    if ((result = SReSyncRequest (sd, type, ssn, settings, base, len, &sis))
	    == NOTOK)
	if (SC_FATAL (sa -> sa_reason))
	    (void) ss2pslose (pb, pi, "SReSyncRequest", sa);
	else {
	    (void) ss2pslose (NULLPB, pi, "SReSyncRequest", sa);
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

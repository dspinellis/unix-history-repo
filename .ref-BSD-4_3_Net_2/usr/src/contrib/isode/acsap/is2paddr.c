/* is2paddr.c - old-style P-ADDR lookup */

#ifndef	lint
static char *rcsid = "$Header: /f/osi/acsap/RCS/is2paddr.c,v 7.2 91/02/22 09:14:38 mrose Interim $";
#endif

/* 
 * $Header: /f/osi/acsap/RCS/is2paddr.c,v 7.2 91/02/22 09:14:38 mrose Interim $
 *
 *
 * $Log:	is2paddr.c,v $
 * Revision 7.2  91/02/22  09:14:38  mrose
 * Interim 6.8
 * 
 * Revision 7.1  90/03/05  23:02:18  mrose
 * touch-up
 * 
 * Revision 7.0  89/11/23  21:22:10  mrose
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
#include "psap.h"
#include "isoaddrs.h"
#include "isoservent.h"

/*  */

struct PSAPaddr *is2paddr (host, service, is)
char   *host,
       *service;
struct isoservent *is;
{
    AEI	    aei;
    struct PSAPaddr *pa;

    if ((aei = str2aei (host, service)) == NULLAEI
	    || (pa = aei2addr (aei)) == NULLPA)
	return NULLPA;

    if (is && strcmp (is -> is_provider, "psap") == 0) {
	if (is -> is_selectlen > PSSIZE)	/* XXX */
	    return NULLPA;

	bcopy (is -> is_selector, pa -> pa_selector,
		pa -> pa_selectlen = is -> is_selectlen);
    }

    return pa;
}

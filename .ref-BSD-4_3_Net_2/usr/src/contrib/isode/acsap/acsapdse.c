/* acsapdse.c - application entity info -- directory service interface */

#ifndef	lint
static char *rcsid = "$Header: /f/osi/acsap/RCS/acsapdse.c,v 7.4 91/02/22 09:14:06 mrose Interim $";
#endif

/* 
 * $Header: /f/osi/acsap/RCS/acsapdse.c,v 7.4 91/02/22 09:14:06 mrose Interim $
 *
 *
 * $Log:	acsapdse.c,v $
 * Revision 7.4  91/02/22  09:14:06  mrose
 * Interim 6.8
 * 
 * Revision 7.3  90/12/11  10:51:56  mrose
 * lock-and-load
 * 
 * Revision 7.2  90/07/09  14:30:29  mrose
 * sync
 * 
 * Revision 7.1  90/07/01  21:01:53  mrose
 * pepsy
 * 
 * Revision 7.0  89/11/23  21:21:47  mrose
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
#include "DSE-types.h"
#include "psap.h"
#include "isoaddrs.h"
#include "tailor.h"

/*    DATA */

static AEInfo aeis;
static struct PSAPaddr pas;

extern	PE    name2value_dase ();

PE	(*acsap_lookup) () = name2value_dase;

/*  */

AEI	str2aei_dse (string, context, ontty, userdn, passwd)
char   *string,
       *context,
       *userdn,
       *passwd;
int	ontty;
{
    char   *alias,
	    name[BUFSIZ];
    PE	    pe;
    register AEI aei = &aeis;
    register struct PSAPaddr *pa = &pas;
    static int first_time = 1;

    if (first_time)
	first_time = 0;
    else {
	AEIFREE (aei);
    }
    bzero ((char *) aei, sizeof *aei);

    if ((alias = alias2name (string)) == NULL)
	alias = string;
    (void) strcpy (name, alias);

    if (acsap_lookup == NULL) {
	PY_advise (NULLCP, "str2aei_dse: acsap_lookup function not set");
	SLOG (addr_log, LLOG_EXCEPTIONS, NULLCP, ("%s", PY_pepy));
	return NULLAEI;
    }

    bzero ((char *) pa, sizeof *pa);
    if (pe = (*acsap_lookup) (name, context, ontty, userdn, passwd,
			      &aei -> aei_ap_title)) {
	if (parse_DSE_PSAPaddr (pe, 1, NULLIP, NULLVP, (char *) pa) == NOTOK) {
	    SLOG (addr_log, LLOG_EXCEPTIONS, NULLCP,
		  ("parse of presentationAddress failed: %s", PY_pepy));
	}
	else
	    PLOG (addr_log, print_DSE_PSAPaddr, pe, "address", 1);

	pe_free (pe);
    }
    else
	SLOG (addr_log, LLOG_EXCEPTIONS, NULLCP,
	      ("directory returns no value"));

    return (aei -> aei_ap_title ? aei : NULLAEI);
}

/*  */

struct PSAPaddr *aei2addr_dse (aei)
AEI	aei;
{
    register struct PSAPaddr *pa;

    if (aei != &aeis) {
	SLOG (addr_log, LLOG_EXCEPTIONS, NULLCP,
	      ("aei2addr_dse cache miss on %s", sprintaei (aei)));

	return NULLPA;
    }

    return ((pa = &pas) -> pa_addr.sa_addr.ta_naddr > 0 ? pa : NULLPA);
}

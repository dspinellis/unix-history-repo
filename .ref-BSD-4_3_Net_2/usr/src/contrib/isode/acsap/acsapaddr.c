/* acsapaddr.c - application entity information -- lookup */

#ifndef	lint
static char *rcsid = "$Header: /f/osi/acsap/RCS/acsapaddr.c,v 7.5 91/02/22 09:14:04 mrose Interim $";
#endif

/* 
 * $Header: /f/osi/acsap/RCS/acsapaddr.c,v 7.5 91/02/22 09:14:04 mrose Interim $
 *
 *
 * $Log:	acsapaddr.c,v $
 * Revision 7.5  91/02/22  09:14:04  mrose
 * Interim 6.8
 * 
 * Revision 7.4  90/12/11  10:51:54  mrose
 * lock-and-load
 * 
 * Revision 7.3  90/10/29  18:37:47  mrose
 * updates
 * 
 * Revision 7.2  90/07/27  08:41:46  mrose
 * update
 * 
 * Revision 7.1  90/07/09  14:30:27  mrose
 * sync
 * 
 * Revision 7.0  89/11/23  21:21:45  mrose
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
#include "tailor.h"


#ifndef	NOSTUB
AEI	str2aei_stub ();
struct PSAPaddr *aei2addr_stub ();
#endif

AEI	str2aei_dse ();
struct PSAPaddr *aei2addr_dse ();

/*    DATA */

#ifndef	NOSTUB
static char fallback1[BUFSIZ],
	    fallback2[BUFSIZ];
#endif

static struct PSAPaddr *(*lookup) () = NULL;

/*  */

/* backwards compatibility... */

static struct mapping {
    char   *m_key;
    char   *m_value;
}	sac2cn[] = {
    "iso ftam", 	"filestore",
    "iso vt",		"terminal",
    "iso cmip",		"mib",
    "isode passwd lookup demo",
			"passwdstore",
    "isode shell",	"shell",
    "IRP Z39.50",	"Z39.50",
    "pp qmgr interface","pp qmgr",

    NULL
};

/*  */

AEI	_str2aei (designator, qualifier, context, interactive, userdn,
		  passwd)
char   *designator,
       *qualifier,
       *context,
       *userdn,
       *passwd;
int	interactive;
{
    AEI	    aei;
    register struct mapping *m;

    if (qualifier == NULLCP) {
	if (context)
	    for (m = sac2cn; m -> m_key; m++)
		if (strcmp (m -> m_key, context) == 0) {
		    qualifier = m -> m_value;
		    break;
		}

	if (qualifier == NULLCP)
	    qualifier = context ? context: "default";
    }

    if (context == NULLCP) {
	for (m = sac2cn; m -> m_key; m++)
	    if (strcmp (m -> m_value, qualifier) == 0) {
		context = m -> m_key;
		break;
	    }

	if (context == NULLCP)
	    context = qualifier;
    }

    isodetailor (NULLCP, 0);
    LLOG (addr_log, LLOG_TRACE,
	  ("str2aei \"%s\" \"%s\" \"%s\" %d",
	   designator, qualifier, context, interactive));

    aei = NULL, lookup = NULL;
    PY_pepy[0] = NULL;

    if (ns_enabled) {
	if (aei = str2aei_dse (designator, context, interactive, userdn,
			       passwd)) {
	    lookup = aei2addr_dse;
#ifndef	NOSTUB
	    (void) strcpy (fallback1, designator);
	    (void) strcpy (fallback2, qualifier);
	    goto out;
#endif
	}
	else
	    SLOG (addr_log, LLOG_EXCEPTIONS, NULLCP,
		  ("DSE lookup of service \"%s\" \"%s\" \"%s\" failed",
		   designator, context, qualifier));
    }

#ifndef	NOSTUB
    if (aei = str2aei_stub (designator, qualifier))
	lookup = aei2addr_stub;
    else
	SLOG (addr_log, LLOG_EXCEPTIONS, NULLCP,
	      ("stub DSE lookup of service \"%s\" \"%s\" \"%s\" failed",
	       designator, context, qualifier));
#endif

#ifndef	NOSTUB
out: ;
#endif
    SLOG (addr_log, LLOG_TRACE, NULLCP,
	  ("str2aei returns %s", aei ? sprintaei (aei) : "NULLAEI"));

    return aei;
}

/*  */

struct PSAPaddr *aei2addr (aei)
AEI	aei;
{
    struct PSAPaddr *pa;

    isodetailor (NULLCP, 0);
    SLOG (addr_log, LLOG_TRACE, NULLCP, ("aei2addr %s", sprintaei (aei)));

    PY_pepy[0] = NULL;

    if (lookup) {
	pa = (*lookup) (aei);
#ifndef	NOSTUB
	if (pa == NULLPA
		&& lookup == aei2addr_dse
		&& (aei = str2aei_stub (fallback1, fallback2))
		&& (pa = aei2addr_stub (aei))) {
	    SLOG (addr_log, LLOG_NOTICE, NULLCP, 
		   ("fallback use of stub DSE succeeded"));
	}
#endif

	lookup = NULL;
    }
    else
	pa = NULLPA;

    SLOG (addr_log, LLOG_TRACE, NULLCP,
	  ("aei2addr returns %s", paddr2str (pa, NULLNA)));

    return pa;
}

/* servbyname.c - getisoserventbyname */

#ifndef	lint
static char *rcsid = "$Header: /f/osi/compat/RCS/servbyname.c,v 7.1 91/02/22 09:15:48 mrose Interim $";
#endif

/* 
 * $Header: /f/osi/compat/RCS/servbyname.c,v 7.1 91/02/22 09:15:48 mrose Interim $
 *
 *
 * $Log:	servbyname.c,v $
 * Revision 7.1  91/02/22  09:15:48  mrose
 * Interim 6.8
 * 
 * Revision 7.0  89/11/23  21:23:26  mrose
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
#include "general.h"
#include "manifest.h"
#include "isoservent.h"
#include "tailor.h"

/*  */

struct isoservent *getisoserventbyname (entity, provider)
char *entity,
     *provider;
{
    register struct isoservent *is;

    isodetailor (NULLCP, 0);
    DLOG (addr_log, LLOG_TRACE,
	  ("getisoserventbyname \"%s\" \"%s\"", entity, provider));

    (void) setisoservent (0);
    while (is = getisoservent ())
	if (strcmp (entity, is -> is_entity) == 0
		&& strcmp (provider, is -> is_provider) == 0)
	    break;
    (void) endisoservent ();

    if (is) {
#ifdef	DEBUG
	if (addr_log -> ll_events & LLOG_DEBUG)
	    _printsrv (is);
#endif
    }
    else
	SLOG (addr_log, LLOG_EXCEPTIONS, NULLCP,
	      ("lookup of local service %s/%s failed", provider, entity));

    return is;
}

/* objectbyname.c - getisobjectbyoid */

#ifndef	lint
static char *rcsid = "$Header: /f/osi/psap/RCS/objectbyoid.c,v 7.1 91/02/22 09:35:52 mrose Interim $";
#endif

/* 
 * $Header: /f/osi/psap/RCS/objectbyoid.c,v 7.1 91/02/22 09:35:52 mrose Interim $
 *
 *
 * $Log:	objectbyoid.c,v $
 * Revision 7.1  91/02/22  09:35:52  mrose
 * Interim 6.8
 * 
 * Revision 7.0  89/11/23  22:12:47  mrose
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
#include "tailor.h"

/*  */

struct isobject *getisobjectbyoid (oid)
register OID	oid;
{
    register struct isobject   *io;

    isodetailor (NULLCP, 0);
#ifdef	DEBUG
    SLOG (addr_log, LLOG_TRACE, NULLCP,
	  ("getisobjectbyoid %s", sprintoid (oid)));
#endif

    (void) setisobject (0);
    while (io = getisobject ())
	if (oid_cmp (oid, &io -> io_identity) == 0)
	    break;
    (void) endisobject ();

    if (io) {
#ifdef	DEBUG
	SLOG (addr_log, LLOG_DEBUG, NULLCP,
	      ("\tODE: \"%s\"\tOID: %s",
		    io -> io_descriptor, sprintoid (&io -> io_identity)));
#endif
    }
    else
	SLOG (addr_log, LLOG_EXCEPTIONS, NULLCP,
	      ("lookup of object %s failed", sprintoid (oid)));

    return io;
}

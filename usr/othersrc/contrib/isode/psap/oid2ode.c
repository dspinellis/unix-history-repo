/* oid2ode.c - object identifier to object descriptor  */

#ifndef	lint
static char *rcsid = "$Header: /f/osi/psap/RCS/oid2ode.c,v 7.2 91/02/22 09:35:54 mrose Interim $";
#endif

/* 
 * $Header: /f/osi/psap/RCS/oid2ode.c,v 7.2 91/02/22 09:35:54 mrose Interim $
 *
 *
 * $Log:	oid2ode.c,v $
 * Revision 7.2  91/02/22  09:35:54  mrose
 * Interim 6.8
 * 
 * Revision 7.1  90/07/09  14:43:47  mrose
 * sync
 * 
 * Revision 7.0  89/11/23  22:12:49  mrose
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

char   *oid2ode_aux (identifier, quoted)
OID	identifier;
int	quoted;
{
    int	    events;
    register struct isobject *io;
    static char buffer[BUFSIZ];
    
    events = addr_log -> ll_events;
    addr_log -> ll_events = LLOG_FATAL;

    io = getisobjectbyoid (identifier);

    addr_log -> ll_events = events;

    if (io) {
	(void) sprintf (buffer, quoted ? "\"%s\"" : "%s",
			io -> io_descriptor);
	return buffer;
    }

    return sprintoid (identifier);
}

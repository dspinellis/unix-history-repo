/* na2str.c - pretty-print NSAPaddr */

#ifndef	lint
static char *rcsid = "$Header: /f/osi/compat/RCS/na2str.c,v 7.2 91/02/22 09:15:37 mrose Interim $";
#endif

/* 
 * $Header: /f/osi/compat/RCS/na2str.c,v 7.2 91/02/22 09:15:37 mrose Interim $
 *
 *
 * $Log:	na2str.c,v $
 * Revision 7.2  91/02/22  09:15:37  mrose
 * Interim 6.8
 * 
 * Revision 7.1  90/07/09  14:32:05  mrose
 * sync
 * 
 * Revision 7.0  89/11/23  21:23:19  mrose
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
#include "isoaddrs.h"

/*    Network Address to String */

char   *na2str (na)
register struct NSAPaddr *na;
{
    switch (na -> na_stack) {
	case NA_TCP: 
	    return na -> na_domain;

	case NA_X25:
	case NA_BRG: 
	    return na -> na_dte;

	case NA_NSAP:
	default:
	    return sel2str (na -> na_address, na -> na_addrlen, 0);
    }
}

/* strdup.c - create a duplicate copy of the given string */

#ifndef lint
static char *rcsid = "$Header: /f/osi/compat/RCS/strdup.c,v 7.3 91/02/22 09:16:05 mrose Interim $";
#endif

/*
 * $Header: /f/osi/compat/RCS/strdup.c,v 7.3 91/02/22 09:16:05 mrose Interim $
 *
 *
 * $Log:	strdup.c,v $
 * Revision 7.3  91/02/22  09:16:05  mrose
 * Interim 6.8
 * 
 * Revision 7.2  90/11/04  19:14:52  mrose
 * update
 * 
 * Revision 7.1  90/10/15  18:19:58  mrose
 * sync
 * 
 * Revision 7.0  89/11/23  21:23:40  mrose
 * Release 6.0
 * 
 */

/*
 *                                NOTICE
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
#include "tailor.h"

/*  */

char   *strdup (str)
register char   *str;
{
	register char *ptr;

	if ((ptr = malloc((unsigned) (strlen (str) + 1))) == NULL){
	    LLOG (compat_log,LLOG_FATAL, ("strdup malloc() failure"));
	    abort ();
	    /* NOTREACHED */
	}

	(void) strcpy (ptr, str);

	return ptr;
}

/* isofiles.c - ISODE files */

#ifndef	lint
static char *rcsid = "$Header: /f/osi/compat/RCS/isofiles.c,v 7.1 91/02/22 09:15:20 mrose Interim $";
#endif

/* 
 * $Header: /f/osi/compat/RCS/isofiles.c,v 7.1 91/02/22 09:15:20 mrose Interim $
 *
 *
 * $Log:	isofiles.c,v $
 * Revision 7.1  91/02/22  09:15:20  mrose
 * Interim 6.8
 * 
 * Revision 7.0  89/11/23  21:23:07  mrose
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
#include "tailor.h"

/*  */

char   *_isodefile (path, file)
char   *path,
       *file;
{
    static char buffer[BUFSIZ];

    isodetailor (NULLCP, 0);	/* not really recursive */

    if (*file == '/'
	    || (*file == '.'
		    && (file[1] == '/'
			    || (file[1] == '.' && file[2] == '/'))))
	(void) strcpy (buffer, file);
    else
	(void) sprintf (buffer, "%s%s", path, file);

    return buffer;
}

/* smalloc.c - error checking malloc */

#ifndef lint
static char *rcsid = "$Header: /f/osi/compat/RCS/smalloc.c,v 7.1 91/02/22 09:15:52 mrose Interim $";
#endif

/*
 * $Header: /f/osi/compat/RCS/smalloc.c,v 7.1 91/02/22 09:15:52 mrose Interim $
 *
 *
 * $Log:	smalloc.c,v $
 * Revision 7.1  91/02/22  09:15:52  mrose
 * Interim 6.8
 * 
 * Revision 7.0  89/11/23  21:23:33  mrose
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

char *
smalloc(size)
int	size;
{
	register char *ptr;

	if ((ptr = malloc((unsigned) size)) == NULL){
	    LLOG (compat_log,LLOG_FATAL, ("malloc() failure"));
	    abort ();
	    /* NOTREACHED */
	}

	return(ptr);
}

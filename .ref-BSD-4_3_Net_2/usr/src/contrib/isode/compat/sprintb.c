/* sprintb.c - sprintf on bits */

#ifndef	lint
static char *rcsid = "$Header: /f/osi/compat/RCS/sprintb.c,v 7.1 91/02/22 09:15:53 mrose Interim $";
#endif

/* 
 * $Header: /f/osi/compat/RCS/sprintb.c,v 7.1 91/02/22 09:15:53 mrose Interim $
 *
 *
 * $Log:	sprintb.c,v $
 * Revision 7.1  91/02/22  09:15:53  mrose
 * Interim 6.8
 * 
 * Revision 7.0  89/11/23  21:23:33  mrose
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

/*  */

char  *sprintb (v, bits)
register int	v;
register char  *bits;
{
    register int    i,
                    j;
    register char   c,
                   *bp;
    static char buffer[BUFSIZ];

    (void) sprintf (buffer, bits && *bits == 010 ? "0%o" : "0x%x", v);
    bp = buffer + strlen (buffer);

    if (bits && *++bits) {
	j = 0;
	*bp++ = '<';
	while (i = *bits++)
	    if (v & (1 << (i - 1))) {
		if (j++)
		    *bp++ = ',';
		for (; (c = *bits) > 32; bits++)
		    *bp++ = c;
	    }
	    else
		for (; *bits > 32; bits++)
		    continue;
	*bp++ = '>';
	*bp = NULL;
    }

    return buffer;
}

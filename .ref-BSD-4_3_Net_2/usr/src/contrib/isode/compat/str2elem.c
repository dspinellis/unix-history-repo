/* str2elem.c - string to list of integers */

#ifndef	lint
static char *rcsid = "$Header: /f/osi/compat/RCS/str2elem.c,v 7.2 91/02/22 09:15:55 mrose Interim $";
#endif

/* 
 * $Header: /f/osi/compat/RCS/str2elem.c,v 7.2 91/02/22 09:15:55 mrose Interim $
 *
 *
 * $Log:	str2elem.c,v $
 * Revision 7.2  91/02/22  09:15:55  mrose
 * Interim 6.8
 * 
 * Revision 7.1  90/11/21  11:29:57  mrose
 * sun
 * 
 * Revision 7.0  89/11/23  21:23:35  mrose
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

#include <ctype.h>
#include <stdio.h>
#include "general.h"
#include "manifest.h"

/*  */

int	str2elem (s, elements)
char   *s;
unsigned int elements[];
{
    register int    i;
    register unsigned int  *ip;
    register char  *cp,
                   *dp;

    ip = elements, i = 0;
    for (cp = s; *cp && i <= NELEM; cp = ++dp) {
	for (dp = cp; isdigit ((u_char) *dp); dp++)
	    continue;
	if ((cp == dp) || (*dp && *dp != '.'))
	    break;
	*ip++ = (unsigned int) atoi (cp), i++;
	if (*dp == NULL)
	    break;
    }
    if (*dp || i >= NELEM)
	return NOTOK;
    *ip = 0;

    return i;
}

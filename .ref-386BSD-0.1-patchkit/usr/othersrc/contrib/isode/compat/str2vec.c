/* str2vec.c - string to vector */

#ifndef	lint
static char *rcsid = "$Header: /f/osi/compat/RCS/str2vec.c,v 7.3 91/02/22 09:16:01 mrose Interim $";
#endif

/* 
 * $Header: /f/osi/compat/RCS/str2vec.c,v 7.3 91/02/22 09:16:01 mrose Interim $
 *
 *
 * $Log:	str2vec.c,v $
 * Revision 7.3  91/02/22  09:16:01  mrose
 * Interim 6.8
 * 
 * Revision 7.2  90/12/23  18:39:41  mrose
 * update
 * 
 * Revision 7.1  90/11/21  11:30:02  mrose
 * sun
 * 
 * Revision 7.0  89/11/23  21:23:39  mrose
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


#define	QUOTE	'\\'

/*  */

int	str2vecX (s, vec, nmask, mask, brk, docomma)
register char  *s,
	      **vec,
    	        brk;
int	nmask,
       *mask,
	docomma;
{
    register int    i;
    char    comma = docomma ? ',' : ' ';

    if (mask)
	*mask = 0;

    for (i = 0; i <= NVEC;) {
	vec[i] = NULL;
	if (brk > 0) {
	    if (i > 0 && *s == brk)
		*s++ = NULL;
	}
	else
	    while (isspace ((u_char) *s) || *s == comma)
		*s++ = NULL;
	if (*s == NULL)
	    break;

	if (*s == '"') {
	    if (i < nmask)
		*mask |= 1 << i;
	    for (vec[i++] = ++s; *s != NULL && *s != '"'; s++)
		if (*s == QUOTE) {
		    if (*++s == '"')
			(void) strcpy (s - 1, s);
		    s--;
		}
	    if (*s == '"')
		*s++ = NULL;
	    continue;
	}
	if (*s == QUOTE && *++s != '"')
	    s--;
	vec[i++] = s;

	if (brk > 0) {
	    if (*s != brk)
		for (s++; *s != NULL && *s != brk; s++)
		    continue;
	}
	else
	    for (s++; *s != NULL && !isspace ((u_char) *s) && *s != comma; s++)
		continue;
    }
    vec[i] = NULL;

    return i;
}

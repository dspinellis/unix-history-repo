/*  Author: Mark Moraes <moraes@csri.toronto.edu> */

/*LINTLIBRARY*/

#include "defs.h"

RCSID("$Id: strdup.c,v 1.8 1993/05/23 03:38:27 moraes Exp $")

/* 
 *  makes a copy of a null terminated string in malloc'ed storage.
 *  returns null if it fails.
 */
char *
strdup(s)
const char *s;
{
	char *cp;

	if (s) {
		cp = (char *) malloc((unsigned) (strlen(s)+1));
		if (cp)
			(void) strcpy(cp, s);
	} else
		cp = (char *) NULL;
	return(cp);
}

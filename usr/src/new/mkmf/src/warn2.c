/* $Header: warn2.c,v 1.1 85/04/01 17:20:41 nicklin Exp $ */

/*
 * Author: Peter J. Nicklin
 */

/*
 * warn2() places an error message and 2 string arguments on the
 * standard error output stream stderr.
 */
#include <stdio.h>
#include "null.h"

extern char *PGN;			/* program name */

warn2(m, s1, s2)
	char *m;			/* warning message */
	char *s1;			/* string argument */
	char *s2;			/* string argument */
{
	if (PGN != NULL && *PGN != '\0')
		fprintf(stderr, "%s: ", PGN);
	fprintf(stderr, m, s1, s2);
	fprintf(stderr, "\n");
}

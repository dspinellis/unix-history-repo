/* $Header: warns.c,v 1.1 85/03/25 11:29:43 nicklin Exp $ */

/*
 * Author: Peter J. Nicklin
 */

/*
 * warns() places an error message and a string argument on the
 * standard error output stream stderr.
 */
#include <stdio.h>
#include "null.h"

extern char *PGN;			/* program name */

warns(m, s)
	char *m;			/* warning message */
	char *s;			/* string argument */
{
	if (PGN != NULL && *PGN != '\0')
		fprintf(stderr, "%s: ", PGN);
	fprintf(stderr, m, s);
	fprintf(stderr, "\n");
}

/* $Header: badopt.c,v 1.2 85/04/02 07:45:43 nicklin Exp $ */

/*
 * Author: Peter J. Nicklin
 */

/*
 * badopt() places a "bad option" error message on the standard error
 * output stream stderr.
 */
#include <stdio.h>
#include "null.h"

extern char *PGN;			/* program name */

badopt(sign, c)
	char c;				/* offending option */
	char sign;			/* '+' or '-' sign preceding option */
{
	if (PGN != NULL && *PGN != '\0')
		fprintf(stderr, "%s: ", PGN);
	fprintf(stderr, "bad option %c%c\n", sign, c);
}

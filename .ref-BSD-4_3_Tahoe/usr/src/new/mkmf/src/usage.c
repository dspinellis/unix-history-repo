/* $Header: usage.c,v 1.2 85/03/27 07:24:08 nicklin Exp $ */

/*
 * Author: Peter J. Nicklin
 */

/*
 * usage() places a usage error message on the standard error
 * output stream stderr.
 */
#include <stdio.h>
#include "null.h"

extern char *PGN;			/* program name */

usage(m)
	char *m;			/* usage error message */
{
	fprintf(stderr, "usage: ");
	if (PGN != NULL && *PGN != '\0')
		fprintf(stderr, "%s ", PGN);
	fprintf(stderr, "%s\n", m);
}

/* $Header$ */

/*
 * Author: Peter J. Nicklin
 */

/*
 * warn() places an error message on the standard error output stream
 * stderr.
 */
#include <stdio.h>

extern char *PGN;			/* program name */

/* VARARGS1 */
warn(fmt, args)
	char *fmt;
{
	if (*PGN != '\0')
		fprintf(stderr, "%s: ", PGN);
	_doprnt(fmt, &args, stderr);
	putc('\n', stderr);
}

/* $Header$ */

/*
 * Author: Peter J. Nicklin
 */

/*
 * fatal() places an error message on the standard error output stream
 * stderr and terminates the process by calling exit(1).
 */
#include <stdio.h>

extern char *PGN;			/* program name */

/* VARARGS1 */
fatal(fmt, args)
	char *fmt;
{
	if (*PGN != '\0')
		fprintf(stderr, "%s: ", PGN);
	_doprnt(fmt, &args, stderr);
	putc('\n', stderr);
	exit(1);
}

/*-
 * Copyright (c) 1979 The Regents of the University of California.
 * All rights reserved.
 *
 * %sccs.include.redist.c%
 */

#ifndef lint
static char sccsid[] = "@(#)PERROR.c	1.3 (Berkeley) %G%";
#endif /* not lint */

#include	<stdio.h>
#include	<signal.h>

/*
 * Routine PERROR is called from the runtime library when a runtime
 * I/O error occurs. Its arguments are a pointer to an error message and 
 * the name of the offending file.
 */
long
PERROR(msg, fname)

	char	*msg, *fname;
{
	PFLUSH();
	fputc('\n',stderr);
	fputs(msg, stderr);
	perror(fname);
	kill(getpid(), SIGTRAP);
	return 0;
}

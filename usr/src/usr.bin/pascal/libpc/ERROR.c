/*-
 * Copyright (c) 1979 The Regents of the University of California.
 * All rights reserved.
 *
 * %sccs.include.redist.c%
 */

#ifndef lint
static char sccsid[] = "@(#)ERROR.c	1.11 (Berkeley) %G%";
#endif /* not lint */

#include	<stdio.h>
#include	<signal.h>

/*
 * Routine ERROR is called from the runtime library when a runtime
 * error occurs. Its arguments are a pointer to an error message and 
 * an error specific piece of data.
 */
long
ERROR(msg, d1, d2)

	char	*msg;
	long	d1, d2;
{
	PFLUSH();
	fputc('\n',stderr);
	fprintf(stderr, msg, d1, d2);
	kill(getpid(), SIGTRAP);
	return d1;
}

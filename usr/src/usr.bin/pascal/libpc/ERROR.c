/* Copyright (c) 1979 Regents of the University of California */

static char sccsid[] = "@(#)ERROR.c 1.9 %G%";

#include	<stdio.h>

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
	SETRACE();
	return d1;
}

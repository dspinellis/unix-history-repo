/* Copyright (c) 1979 Regents of the University of California */

static char sccsid[] = "@(#)ERROR.c 1.8 6/10/81";

#include	<stdio.h>
#include	<signal.h>
#include	"h00vars.h"

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
	SETRACE();
	/*
	 * Historical anomaly
	 */
	if ((int)msg == 5) {
		fprintf(stderr, "Label of %D not found in case\n", d1);
		return d1;
	}
	fprintf(stderr, msg, d1, d2);
	return d1;
}

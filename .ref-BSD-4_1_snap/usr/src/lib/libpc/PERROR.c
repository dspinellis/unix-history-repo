/* Copyright (c) 1979 Regents of the University of California */

static char sccsid[] = "@(#)PERROR.c 1.1 6/10/81";

#include	<stdio.h>
#include	<signal.h>
#include	"h00vars.h"

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
	SETRACE();
	fputs(msg, stderr);
	perror(fname);
	return 0;
}

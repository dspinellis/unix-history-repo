/*-
 * Copyright (c) 1992, 1993
 *	The Regents of the University of California.  All rights reserved.
 *
 * This code is derived from software contributed to Berkeley by
 * Rodney Ruddock of the University of Guelph.
 *
 * %sccs.include.redist.c%
 */

#ifndef lint
static char sccsid[] = "@(#)equal.c	8.1 (Berkeley) %G%";
#endif /* not lint */

#include <sys/types.h>

#include <regex.h>
#include <setjmp.h>
#include <stdio.h>

#ifdef DBI
#include <db.h>
#endif

#include "ed.h"
#include "extern.h"

/*
 * Print out what the line number of the address given is; default to
 * end-of-buffer ($).
 */
void
equal(inputt, errnum)
	FILE *inputt;
	int *errnum;
{
	if (End_default)
		Start = bottom;
	else
		Start = End;
	Start_default = End_default = 0;

	if (rol(inputt, errnum))
		return;

	(void)printf("%d\n", line_number(Start));
	*errnum = 1;
}

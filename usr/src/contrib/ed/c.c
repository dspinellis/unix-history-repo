/*-
 * Copyright (c) 1992 The Regents of the University of California.
 * All rights reserved.
 *
 * This code is derived from software contributed to Berkeley by
 * Rodney Ruddock of the University of Guelph.
 *
 * %sccs.include.redist.c%
 */

#ifndef lint
static char sccsid[] = "@(#)c.c	5.2 (Berkeley) %G%";
#endif /* not lint */

#include <sys/types.h>

#include <db.h>
#include <regex.h>
#include <setjmp.h>
#include <stdio.h>

#include "ed.h"
#include "extern.h"

/*
 * This deletes the range of lines specified and then sets up things
 * for the central input routine.
 */

void
c(inputt, errnum)
	FILE *inputt;
	int *errnum;
{
	if (start_default && End_default)
		start = End = current;
	else
		if (start_default)
			start = End;
	if (start == NULL) {
		*errnum = -1;
		return;
	}
	start_default = End_default = 0;

	/* first delete the lines */
	d(inputt, errnum);
	if (*errnum < 0)
		return;
	*errnum = 0;

	if ((current != NULL) && (current != bottom))
		current = current->above;
	if (sigint_flag)
		SIGINT_ACTION;
	add_flag = 1;
	start_default = End_default = 1;
	/* now get the "change" lines */
	input_lines(inputt, errnum);
	add_flag = 0;
}

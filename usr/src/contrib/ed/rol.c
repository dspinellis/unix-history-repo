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
static char sccsid[] = "@(#)rol.c	5.2 (Berkeley) %G%";
#endif /* not lint */

#include <sys/types.h>

#include <db.h>
#include <regex.h>
#include <setjmp.h>
#include <stdio.h>
#include <string.h>

#include "ed.h"
#include "extern.h"

/*
 * After the command check the rest of the line to see nothing illegal
 * is following. Any single instance of a printsfx suffix is the only
 * legal thing to follow ( that is, a 'l', 'n', or 'p'). If a suffix
 * occurs the execution of that suffix occurs back in the cmd_loop
 * function after the command that called this function finishes
 * successfully.
 */
int
rol(inputt, errnum)
	FILE *inputt;
	int *errnum;
{
	ss = getc(inputt);
	printsfx = 0;

	/* Only one of the suffix is allowed. */
	if (ss == 'p')
		printsfx = 1;
	else
		if (ss == 'n')
			printsfx = 2;
		else
			if (ss == 'l')
				printsfx = 4;
			else
				ungetc(ss, inputt);

	for (;;) {
		ss = getc(inputt);
		if ((ss != ' ') && (ss != '\n') && (ss != EOF)) {
			*errnum = -1;
			strcpy(help_msg, "illegal command option");
			return (1);
		}
		if ((ss == '\n') || (ss == EOF))
			break;
	}

	/* Rest-of-line was okay. */
	return (0);
}

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
static char sccsid[] = "@(#)p.c	8.1 (Berkeley) %G%";
#endif /* not lint */

#include <sys/types.h>

#include <regex.h>
#include <setjmp.h>
#include <stdio.h>
#include <string.h>

#ifdef DBI
#include <db.h>
#endif

#include "ed.h"
#include "extern.h"

/*
 * Both the n and p code are here because they're almost identical.
 * Print out the line. If it's n print the line number, tab, and then
 * the line.
 */
void
p(inputt, errnum, flag)
	FILE *inputt;
	int *errnum, flag;
{
	int l_ln=0;

	if (Start_default && End_default)
		Start = End = current;
	else
		if (Start_default)
			Start = End;
	Start_default = End_default = 0;

	if (Start == NULL) {
		strcpy(help_msg, "buffer empty");
		*errnum = -1;
		return;
	}
	if (rol(inputt, errnum))	/* For "command-suffix pairs". */
		return;

	if (flag == 1)
		l_ln = line_number(Start);
	current = Start;
	for (;;) {
		/* Print out the lines. */
		if (current == NULL)
			break;
		get_line(current->handle, current->len);
		if (sigint_flag && (!sigspecial))
			SIGINT_ACTION;
		if (flag == 1)		/* When 'n'. */
			printf("%d\t", l_ln++);
		fwrite(text, sizeof(char), current->len, stdout);
		putchar('\n');
		if (current == End)
			break;
		current = current->below;
	}

	*errnum = 1;
}

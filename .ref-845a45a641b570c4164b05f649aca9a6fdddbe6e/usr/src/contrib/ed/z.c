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
static char sccsid[] = "@(#)z.c	8.1 (Berkeley) %G%";
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
 * This prints out the next group of lines (as spec'd by the skicky
 * number; 22 by default). It's really useful for scrolling in chunks
 * through the buffer (better than l, n, or p). Shame on POSIX for
 * not including it; yaaa! BSD for keeping it! :-)
 */
void
z(inputt, errnum)
	FILE *inputt;
	int *errnum;
{
	register int l_cnt;

	if (current == NULL) {
		*errnum = -1;
		strcpy(help_msg, "buffer empty");
		return;
	}
	/* Set zsnum if need be here. */
	ss = getc(inputt);
	if ((ss > 48) && (ss < 57))
		/* Default set in main. */
		zsnum = dig_num_conv(inputt, errnum);
	else
		if ((ss != '\n') && (ss != EOF)) {
			ungetc(ss, inputt);
			if (rol(inputt, errnum))
				return;
		}
	if (top == NULL) {
		strcpy(help_msg, "buffer empty");
		*errnum = -1;
		ungetc('\n', inputt);
		return;
	}
	if (End_default) {
		if ((current->below) != NULL)
			Start = current->below;
		else {
			strcpy(help_msg, "at end of buffer");
			*errnum = -1;
			ungetc('\n', inputt);
			return;
		}
	} else
		Start = End;
	if (Start == NULL) {
		strcpy(help_msg, "bad address");
		*errnum = -1;
		ungetc('\n', inputt);
		return;
	}
	Start_default = End_default = 0;

	current = Start;
	l_cnt = 1;		/* Yes, set to = 1. */
	while (1) {
		/* Scroll-out the next 'zsnum' of lines or until bottom. */
		if (current == NULL)
			break;
		if (sigint_flag && (!sigspecial))
			SIGINT_ACTION;
		get_line(current->handle, current->len);
		printf("%s\n", text);
		if (current == bottom)
			break;
		l_cnt++;
		if (zsnum < l_cnt)
			break;
		current = current->below;
	}
	*errnum = 1;
}

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
static char sccsid[] = "@(#)z.c	5.2 (Berkeley) %G%";
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
		strcpy(help_msg, "no lines in buffer");
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
	if (sigint_flag)
		SIGINT_ACTION;
	if (End_default) {
		if ((current->below) != NULL)
			start = current->below;
		else {
			strcpy(help_msg, "at end of buffer");
			*errnum = -1;
			ungetc('\n', inputt);
			return;
		}
	} else
		start = End;
	if (start == NULL) {
		strcpy(help_msg, "bad address");
		*errnum = -1;
		ungetc('\n', inputt);
		return;
	}
	start_default = End_default = 0;

	current = start;
	l_cnt = 1;		/* Yes, set to = 1. */
	while (1) {
		/* Scroll-out the next 'zsnum' of lines or until bottom. */
		if (sigint_flag)
			SIGINT_ACTION;
		if (current == NULL)
			break;
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

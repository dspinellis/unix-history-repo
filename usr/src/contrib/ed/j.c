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
static char sccsid[] = "@(#)j.c	5.3 (Berkeley) %G%";
#endif /* not lint */

#include <sys/types.h>

#include <regex.h>
#include <setjmp.h>
#include <stdio.h>
#include <stdlib.h>
#include <string.h>

#ifdef DBI
#include <db.h>
#endif

#include "ed.h" 
#include "extern.h"

/*
 * Join the spec'd lines onto one line. Make the new line from the
 * old lines and then delete the old ones (undo-able).
 */

void
j(inputt, errnum)
	FILE *inputt;
	int *errnum;
{
	LINE *l_ptr, *l_temp_line;
	long l_ttl = 0;
	char *l_temp1;

	if (start_default && End_default) {
		start = current;
		*errnum = 1;
		if (start == NULL) {
			strcpy(help_msg, "buffer empty");
			*errnum = -1;
			return;
		} else
			if ((start->below) != NULL)
				End = start->below;
		else
			return;
	} else
		if (start_default) {
			if (rol(inputt, errnum))
				return;

			if (End == NULL) {
				strcpy(help_msg, "bad address");
				*errnum = -1;
			} else {
#ifdef BSD
				/*
				 * For BSD a 'j' with one address sets
				 * "current" to that line
				 */
				if (start)
					current = start;
#endif
				*errnum = 1;
			}
			return;
		}

	if (start == NULL) {
		strcpy(help_msg, "buffer empty");
		*errnum = -1;
		return;
	}

	start_default = End_default = 0;

	if (rol(inputt, errnum))
		return;

	if (g_flag == 0)
		u_clr_stk();
	u_set = 1;		/* set for d */

	/* Figure out what the length of the joined lines will be. */
	for (l_ptr = start; l_ptr != (End->below); l_ptr = (l_ptr->below))
		l_ttl = l_ptr->len + l_ttl;

	if (l_ttl > nn_max) {
		/*
		 * The new line is bigger than any so far, so make more
		 * space.
		 */
		sigspecial++;
		free(text);
		nn_max = l_ttl;
		text = calloc(l_ttl + 2, sizeof(char));
		sigspecial--;
		if (text == NULL) {
			*errnum = -1;
			strcpy(help_msg, "out of memory error");
			return;
		}
	}
	l_temp1 = calloc(l_ttl + 2, sizeof(char));
	if (l_temp1 == NULL) {
		*errnum = -1;
		strcpy(help_msg, "out of memory error");
		return;
	}
	l_temp1[0] = '\0';

	l_ptr = start;

	sigspecial++;
	for (;;) {
		/* Get each line and catenate. */
		get_line(l_ptr->handle, l_ptr->len);
		if (sigint_flag && (!sigspecial))
			goto point;
		strcat(l_temp1, text);
		l_ptr = l_ptr->below;
		if (l_ptr == End->below)
			break;
	}

	l_temp_line = malloc(sizeof(LINE));
	if (l_temp_line == NULL) {
		*errnum = -1;
		strcpy(help_msg, "out of memory error");
		return;
	}
	(l_temp_line->len) = l_ttl;
	/* Add the new line to the buffer. */
	(l_temp_line->handle) = add_line(l_temp1, l_ttl);
	if (start == top) {
		top = l_temp_line;
		(l_temp_line->above) = NULL;
	} else {
		(l_temp_line->above) = start->above;
		u_add_stk(&(l_temp_line->above->below));
		(l_temp_line->above->below) = l_temp_line;
	}
	(l_temp_line->below) = start;
	u_add_stk(&(start->above));
	(start->above) = l_temp_line;

	sigspecial--;
	if (sigint_flag && (!sigspecial))
		goto mk;
	ungetc(ss, inputt);
	/* Delete the lines used to make the joined line. */
	join_flag = 1;
	d(inputt, errnum);
	join_flag = 0;
	if (*errnum < 0)
		return;
	*errnum = 0;
mk:
	current = l_temp_line;

	*errnum = 1;

point:	u_set = 0;
	free(l_temp1);
}

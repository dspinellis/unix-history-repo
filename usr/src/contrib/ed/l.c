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
static char sccsid[] = "@(#)l.c	5.2 (Berkeley) %G%";
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
 * This is the list command. It's not wrapped in with n and p because
 * of the unambiguous printing needed.
 */
void
l(inputt, errnum)
	FILE *inputt;
	int *errnum;
{
	int l_cnt, l_len = 1;

	if (start_default && End_default)
		start = End = current;
	else
		if (start_default)
			start = End;

	if (start == NULL) {
		strcpy(help_msg, "bad address");
		*errnum = -1;
		return;
	}
	start_default = End_default = 0;

	if (rol(inputt, errnum))	/* For "command-suffix pairs". */
		return;

	current = start;
	for (;;) {
		/*
		 * Print out the line character-by-character and split the
		 * line when line length is at line_length.
		 */
		if (sigint_flag)
			SIGINT_ACTION;
		if (current == NULL)
			break;
		get_line(current->handle, current->len);
		for (l_cnt = 0; l_cnt < current->len; l_cnt++, l_len += 2) {
			/* Check if line needs to be broken first. */
			if ((l_len) % line_length == 0)
				putchar('\n');
			else switch (text[l_cnt]) {
			case '\b':	/* backspace (cntl-H) */
				fwrite("\\b", sizeof(char), 2, stdout);
				break;
			case '\t':	/* horizontal tab */
				fwrite("\\t", sizeof(char), 2, stdout);
				break;
			case '\n':	/* newline (not that there is one). */
				fwrite("\\n", sizeof(char), 2, stdout);
				break;
			case '\v':	/* vertical tab */
				fwrite("\\v", sizeof(char), 2, stdout);
				break;
			case '\f':	/* form feed */
				fwrite("\\f", sizeof(char), 2, stdout);
				break;
			case '\r':	/* return */
				fwrite("\\r", sizeof(char), 2, stdout);
				break;
			default:
				if ((text[l_cnt] < 32) ||
				    (text[l_cnt] > 126)) {
					putchar('\\');
					putchar(text[l_cnt] / 64 + '0');
					putchar(text[l_cnt] / 8 + '0');
					putchar(text[l_cnt] % 8 + '0');
					l_len += 2;
				} else if (text[l_cnt] == '\\')
					fwrite("\\\\", sizeof(char), 2, stdout);
				else {
					l_len--;
					putchar(text[l_cnt]);
				}
				break;
			}
		}
		l_len = 1;
		putchar('\n');
		if (current == End)
			break;
		current = current->below;
	}
	*errnum = 1;
}

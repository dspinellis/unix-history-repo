/*-
 * Copyright (c) 1992, 1993
 *	The Regents of the University of California.  All rights reserved.
 *
 * This code is derived from software contributed to Berkeley by
 * Rodney Ruddock of the University of Guelph.
 *
 * Redistribution and use in source and binary forms, with or without
 * modification, are permitted provided that the following conditions
 * are met:
 * 1. Redistributions of source code must retain the above copyright
 *    notice, this list of conditions and the following disclaimer.
 * 2. Redistributions in binary form must reproduce the above copyright
 *    notice, this list of conditions and the following disclaimer in the
 *    documentation and/or other materials provided with the distribution.
 * 3. All advertising materials mentioning features or use of this software
 *    must display the following acknowledgement:
 *	This product includes software developed by the University of
 *	California, Berkeley and its contributors.
 * 4. Neither the name of the University nor the names of its contributors
 *    may be used to endorse or promote products derived from this software
 *    without specific prior written permission.
 *
 * THIS SOFTWARE IS PROVIDED BY THE REGENTS AND CONTRIBUTORS ``AS IS'' AND
 * ANY EXPRESS OR IMPLIED WARRANTIES, INCLUDING, BUT NOT LIMITED TO, THE
 * IMPLIED WARRANTIES OF MERCHANTABILITY AND FITNESS FOR A PARTICULAR PURPOSE
 * ARE DISCLAIMED.  IN NO EVENT SHALL THE REGENTS OR CONTRIBUTORS BE LIABLE
 * FOR ANY DIRECT, INDIRECT, INCIDENTAL, SPECIAL, EXEMPLARY, OR CONSEQUENTIAL
 * DAMAGES (INCLUDING, BUT NOT LIMITED TO, PROCUREMENT OF SUBSTITUTE GOODS
 * OR SERVICES; LOSS OF USE, DATA, OR PROFITS; OR BUSINESS INTERRUPTION)
 * HOWEVER CAUSED AND ON ANY THEORY OF LIABILITY, WHETHER IN CONTRACT, STRICT
 * LIABILITY, OR TORT (INCLUDING NEGLIGENCE OR OTHERWISE) ARISING IN ANY WAY
 * OUT OF THE USE OF THIS SOFTWARE, EVEN IF ADVISED OF THE POSSIBILITY OF
 * SUCH DAMAGE.
 */

#ifndef lint
static char sccsid[] = "@(#)l.c	8.1 (Berkeley) 5/31/93";
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
 * This is the list command. It's not wrapped in with n and p because
 * of the unambiguous printing need.
 */
void
l(inputt, errnum)
	FILE *inputt;
	int *errnum;
{
	int l_cnt, l_len = 1;

	if (Start_default && End_default)
		Start = End = current;
	else
		if (Start_default)
			Start = End;

	if (Start == NULL) {
		strcpy(help_msg, "empty buffer");
		*errnum = -1;
		return;
	}
	Start_default = End_default = 0;

	if (rol(inputt, errnum))	/* For "command-suffix pairs". */
		return;

	current = Start;
	for (;;) {
		/*
		 * Print out the line character-by-character and split the
		 * line when line length is at line_length.
		 */
		if (current == NULL)
			break;
		get_line(current->handle, current->len);
		if (sigint_flag && (!sigspecial))
			SIGINT_ACTION;
		for (l_cnt = 0; l_cnt < current->len; l_cnt++, l_len += 2) {
			/* Check if line needs to be broken first. */
			if (l_len > line_length) {
				putchar('\n');
				l_len = 0;
			}
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
					putchar(((text[l_cnt] & 0xC0) >> 6)
					    + '0');
					putchar(((text[l_cnt] & 0x38) >> 3)
					    + '0');
					putchar((text[l_cnt] & 0x07) + '0');
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

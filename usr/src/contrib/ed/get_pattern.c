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
static char sccsid[] = "@(#)get_pattern.c	8.1 (Berkeley) 5/31/93";
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
 * This is for getting RE and replacement patterns for any command
 * that uses RE's and replacements.
 */
char *
get_pattern(delim, inputt, errnum, flag)
	int delim, *errnum, flag;
	FILE *inputt;
{
	static int l_max = 510;
	int l_cnt = 1;
	char *l_pat, *l_pat_tmp;

	/* get a "reasonable amount of space for the RE */
	l_pat = calloc(l_max + 2, sizeof(char));
	if (l_pat == NULL) {
		*errnum = -3;
		strcpy(help_msg, "out of memory error");
		return (NULL);
	}
	l_pat[0] = delim;

	if ((delim == ' ') || (delim == '\n')) {
		if (delim == '\n')
			ungetc(delim, inputt);
		strcpy(help_msg, "illegal delimiter");
		*errnum = -2;
		return (l_pat);
	}
	for (;;) {
		ss = getc(inputt);
		if (ss == '\\') {
			ss = getc(inputt);
			if ((ss == delim) || ((flag == 1) && (ss == '\n')))
				l_pat[l_cnt] = ss;
			else {
				l_pat[l_cnt] = '\\';
				l_pat[++l_cnt] = ss;
			}
			goto leap;
		} else
			if ((ss == '\n') || (ss == EOF)) {
				ungetc(ss, inputt);
				strcpy(help_msg, "no closing delimiter found");
				*errnum = -1;
				/* This is done for s's backward compat. */
				l_pat[l_cnt] = '\0';
				return (l_pat);
			}
		if (ss == delim)
			break;

		l_pat[l_cnt] = ss;

leap:		if (l_cnt > l_max) {
			/* The RE is really long; get more space for it. */
			l_max = l_max + 256;
			l_pat_tmp = l_pat;
			l_pat = calloc(l_max + 2, sizeof(char));
			if (l_pat == NULL) {
				*errnum = -3;
				strcpy(help_msg, "out of memory error");
				return (NULL);
			}
			memmove(l_pat, l_pat_tmp, l_cnt);
			free(l_pat_tmp);
		}
		l_cnt++;
	}
	l_pat[l_cnt] = '\0';
	*errnum = 0;
	/*
	 * Send back the pattern.  l_pat[0] has the delimiter in it so the RE
	 * really starts at l_pat[1]. It's done this way for the special forms
	 * of 's' (substitute).
	 */
	return (l_pat);
}

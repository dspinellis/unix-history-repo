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
static char sccsid[] = "@(#)k.c	8.1 (Berkeley) 5/31/93";
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
 * This the mark command (k); see ed(1).
 */

void
set_mark(inputt, errnum)
	FILE *inputt;
	int *errnum;
{
	int l_mark;

	l_mark = getc(inputt);
	if (End_default == 1)
		End = current;
	if (End == NULL) {
		strcpy(help_msg, "bad address");
		*errnum = -1;
		ungetc('\n', inputt);
		return;
	}
	Start_default = End_default = 0;

	/*
	 * The marks have to be "a" to "z" (inclusive); that is, ye olde
	 * portable character set (ASCII) lower case alphabet.
	 */
	if ((l_mark < 97) || (l_mark > 122) || (End == NULL)) {
		strcpy(help_msg, "illegal mark character");
		*errnum = -1;
		return;
	}
	l_mark = l_mark - 97;
	(mark_matrix[l_mark].address) = End;

	if (rol(inputt, errnum))
		return;

	*errnum = 1;
}


/*
 * This gets the address of a marked line.
 */
LINE *
get_mark(inputt, errnum)
	FILE *inputt;
	int *errnum;
{
	int l_mark;

	l_mark = getc(inputt);
	/* Ditto above comment. */
	if ((l_mark < 97) || (l_mark > 122)) {
		strcpy(help_msg, "illegal mark character");
		*errnum = -1;
		return (NULL);
	}
	l_mark = l_mark - 97;
	*errnum = 0;
	return (mark_matrix[l_mark].address);
}


/*
 * This is for the restoration of marks during an undo.
 */
void
ku_chk(begi, fini, val)
	LINE *begi, *fini, *val;
{
	register int l_cnt;
	LINE *l_midd;

	l_midd = begi;
	while (l_midd != NULL) {
		for (l_cnt = 0; l_cnt < 26; l_cnt++)
			if (mark_matrix[l_cnt].address == l_midd) {
				u_add_stk(&(mark_matrix[l_cnt].address));
				(mark_matrix[l_cnt].address) = val;
			}
		if (l_midd == fini)
			break;
		l_midd = l_midd->below;
	}
}

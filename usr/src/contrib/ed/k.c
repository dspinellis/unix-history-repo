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
static char sccsid[] = "@(#)k.c	5.2 (Berkeley) %G%";
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
	start_default = End_default = 0;

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
get_mark(errnum)
	int *errnum;
{
	int l_mark;

	l_mark = getchar();
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

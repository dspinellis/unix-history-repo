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
static char sccsid[] = "@(#)q.c	5.2 (Berkeley) %G%";
#endif /* not lint */

#include <sys/types.h>

#include <db.h>
#include <regex.h>
#include <setjmp.h>
#include <stdio.h>
#include <stdlib.h>
#include <string.h>
#include <unistd.h>

#include "ed.h"
#include "extern.h"

/*
 * End this editting session and exit with saving the buffer. If no
 * write has occurred since the last buffer modification a warning
 * is given once ('Q' over-rides the warning).
 */
void
q(inputt, errnum)
	FILE *inputt;
	int *errnum;
{
	register int l_ss = ss;
	int l_which;			/* Which is it? 'q' or 'Q'. */

	l_which = ss;
	for (;;) {
		l_ss = getc(inputt);
		if ((l_ss != ' ') && (l_ss != '\n') && (l_ss != EOF)) {
			*errnum = -1;
			strcpy(help_msg, "illegal command option");
			return;
		}
		if ((l_ss == '\n') || (l_ss == EOF))
			break;
	}

	ungetc(l_ss, inputt);
	/* Note: 'Q' will bypass this if stmt., which warns of no save. */
	if ((change_flag == 1L) && (l_which == 'q')) {
		change_flag = 0L;
		strcpy(help_msg, "buffer changes not saved");
		*errnum = -1;
		ss = l_ss;
		return;
	}
	/* Do cleanup; should it be even bothered?? */
	start = top;
	End = bottom;
	start_default = End_default = 0;

	/* We don't care about the returned errnum val anymore. */
	d(inputt, errnum);
	u_clr_stk();
	free(text);
	free(filename_current);
	(dbhtmp->close) (dbhtmp);	/* Overhead as the cache is flushed. */
	unlink(template);
	exit(0);
}

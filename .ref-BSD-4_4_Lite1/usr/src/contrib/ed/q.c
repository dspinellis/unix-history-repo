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
static char sccsid[] = "@(#)q.c	8.1 (Berkeley) 5/31/93";
#endif /* not lint */

#include <sys/types.h>

#include <regex.h>
#include <setjmp.h>
#include <stdio.h>
#include <stdlib.h>
#include <string.h>
#include <unistd.h>

#ifdef DBI
#include <db.h>
#endif

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

	sigspecial = 1; /* yes, 1, because we want to ensure it's on */
	sigspecial2 = 0;

	l_which = ss;
	if (ss != -1) {
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
	}
	/* Note: 'Q' will bypass this if stmt., which warns of no save. */
	if ((change_flag == 1L) && ((l_which == 'q') || (l_which == -1)) ) {
		change_flag = 0L;
		strcpy(help_msg, "buffer changes not saved");
		*errnum = -1;
		ss = l_ss;
		if (l_which == EOF)
			ungetc('\n', inputt);
		return;
	}
	/* Do cleanup; should it be even bothered?? */
	Start = top;
	End = bottom;
	Start_default = End_default = 0;

	/* We don't care about the returned errnum val anymore. */
	if (l_which == EOF)
		ungetc('\n', inputt);
	d(inputt, errnum);
	u_clr_stk();
	free(text);
	free(filename_current);
#ifdef STDIO
	fclose(fhtmp);
#endif
#ifdef DBI
	(dbhtmp->close) (dbhtmp);	/* Overhead as the cache is flushed. */
#endif
#ifndef MEMORY
	unlink(template);
#endif
	exit(exit_code);
}

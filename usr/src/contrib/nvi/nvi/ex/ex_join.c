/*-
 * Copyright (c) 1992, 1993
 *	The Regents of the University of California.  All rights reserved.
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
static char sccsid[] = "@(#)ex_join.c	8.1 (Berkeley) 6/9/93";
#endif /* not lint */

#include <sys/types.h>

#include <ctype.h>
#include <stdlib.h>
#include <string.h>

#include "vi.h"
#include "excmd.h"

/*
 * ex_join -- :[line [,line]] j[oin][!] [count] [flags]
 *	Join lines.
 */
int
ex_join(sp, ep, cmdp)
	SCR *sp;
	EXF *ep;
	EXCMDARG *cmdp;
{
	recno_t from, to;
	size_t blen, clen, len, tlen;
	int echar, first;
	char *bp, *p, *tbp;

	from = cmdp->addr1.lno;
	to = cmdp->addr2.lno;

	/* Check for no lines to join. */
	if ((p = file_gline(sp, ep, from + 1, &len)) == NULL) {
		msgq(sp, M_ERR, "No remaining lines to join.");
		return (1);
	}

	GET_SPACE(sp, bp, blen, 256);

	clen = tlen = 0;
        for (first = 1, from = cmdp->addr1.lno,
	    to = cmdp->addr2.lno + 1; from <= to; ++from) {
		/*
		 * Get next line.  Historic versions of vi allowed "10J" while
		 * less than 10 lines from the end-of-file, so we do too.
		 */
		if ((p = file_gline(sp, ep, from, &len)) == NULL)
			break;

		/* Empty lines just go away. */
		if (len == 0)
			continue;

		/*
		 * Get more space if necessary.  Note, tlen isn't the length
		 * of the new line, it's roughly the amount of space needed.
		 * tbp - bp is the length of the new line.
		 */
		tlen += len + 2;
		ADD_SPACE(sp, bp, blen, tlen);
		tbp = bp + clen;

		/*
		 * Historic practice:
		 *
		 * If force specified, join without modification.
		 * If the current line ends with whitespace, strip leading
		 *    whitespace from the joined line.
		 * If the next line starts with a ), do nothing.
		 * If the current line ends with ., ? or !, insert two spaces.
		 * Else, insert one space.
		 *
		 * Echar is the last character in the last line joined.
		 */
		if (!first && !F_ISSET(cmdp, E_FORCE)) {
			if (isspace(echar))
				for (; len && isspace(*p); --len, ++p);
			else if (p[0] != ')') {
				if (strchr(".?!", echar))
					*tbp++ = ' ';
				*tbp++ = ' ';
				++clen;
				for (; len && isspace(*p); --len, ++p);
			}
		} else
			first = 0;
			
		if (len != 0) {
			memmove(tbp, p, len);
			tbp += len;
			clen += len;
			echar = p[len - 1];
		} else
			echar = ' ';

		/*
		 * Historic practice for vi was to put the cursor at the first
		 * inserted whitespace character, if there was one, or the
		 * first character of the joined line, if there wasn't.  If
		 * a count was specified, the cursor was moved as described
		 * for the first line joined, ignoring subsequent lines.  If
		 * the join was a ':' command, the cursor was placed at the
		 * first non-blank character of the line unless the cursor was
		 * "attracted" to the end of line when the command was executed
		 * in which case it moved to the new end of line.  There are
		 * probably several more special cases, but frankly, my dear,
		 * I don't give a damn.  This implementation puts the cursor
		 * on the first inserted whitespace character or the first
		 * character of the joined line, regardless.  Note, if the
		 * cursor isn't on the joined line (possible with : commands),
		 * it is reset to the starting line.
		 */
		sp->cno = (tbp - bp) - len - 1;
	}
	sp->lno = cmdp->addr1.lno;

	/* Delete the joined lines. */
        for (from = cmdp->addr1.lno, to = cmdp->addr2.lno + 1; to > from; --to)
		if (file_dline(sp, ep, to))
			goto err;
		
	/* Reset the original line. */
	if (file_sline(sp, ep, from, bp, tbp - bp)) {
err:		FREE_SPACE(sp, bp, blen);
		return (1);
	}
	FREE_SPACE(sp, bp, blen);

	sp->rptlines[L_JOINED] += cmdp->addr2.lno - cmdp->addr1.lno;

	F_SET(sp, S_AUTOPRINT);
	return (0);
}

/*-
 * Copyright (c) 1992, 1993, 1994
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
static char sccsid[] = "@(#)ex_shift.c	9.2 (Berkeley) 11/10/94";
#endif /* not lint */

#include <sys/types.h>
#include <sys/queue.h>
#include <sys/time.h>

#include <bitstring.h>
#include <limits.h>
#include <signal.h>
#include <stdio.h>
#include <stdlib.h>
#include <string.h>
#include <termios.h>

#include "compat.h"
#include <db.h>
#include <regex.h>

#include "vi.h"
#include "excmd.h"

enum which {LEFT, RIGHT};
static int shift __P((SCR *, EXCMDARG *, enum which));

int
ex_shiftl(sp, cmdp)
	SCR *sp;
	EXCMDARG *cmdp;
{
	return (shift(sp, cmdp, LEFT));
}

int
ex_shiftr(sp, cmdp)
	SCR *sp;
	EXCMDARG *cmdp;
{
	return (shift(sp, cmdp, RIGHT));
}

static int
shift(sp, cmdp, rl)
	SCR *sp;
	EXCMDARG *cmdp;
	enum which rl;
{
	recno_t from, to;
	size_t blen, len, newcol, newidx, oldcol, oldidx, sw;
	int curset;
	char *p, *bp, *tbp;

	NEEDFILE(sp, cmdp->cmd);

	if (O_VAL(sp, O_SHIFTWIDTH) == 0) {
		msgq(sp, M_INFO, "155|shiftwidth option set to 0");
		return (0);
	}

	/* Copy the lines being shifted into the unnamed buffer. */
	if (cut(sp, NULL, &cmdp->addr1, &cmdp->addr2, CUT_LINEMODE))
		return (1);

	/*
	 * The historic version of vi permitted the user to string any number
	 * of '>' or '<' characters together, resulting in an indent of the
	 * appropriate levels.  There's a special hack in ex_cmd() so that
	 * cmdp->argv[0] points to the string of '>' or '<' characters.
	 *
	 * Q: What's the difference between the people adding features
	 *    to vi and the Girl Scouts?
	 * A: The Girl Scouts have mint cookies and adult supervision.
	 */
	for (p = cmdp->argv[0]->bp, sw = 0; *p == '>' || *p == '<'; ++p)
		sw += O_VAL(sp, O_SHIFTWIDTH);

	GET_SPACE_RET(sp, bp, blen, 256);

	curset = 0;
	for (from = cmdp->addr1.lno, to = cmdp->addr2.lno; from <= to; ++from) {
		if ((p = file_gline(sp, from, &len)) == NULL)
			goto err;
		if (!len) {
			if (sp->lno == from)
				curset = 1;
			continue;
		}

		/*
		 * Calculate the old indent amount and the number of
		 * characters it used.
		 */
		for (oldidx = 0, oldcol = 0; oldidx < len; ++oldidx)
			if (p[oldidx] == ' ')
				++oldcol;
			else if (p[oldidx] == '\t')
				oldcol += O_VAL(sp, O_TABSTOP) -
				    oldcol % O_VAL(sp, O_TABSTOP);
			else
				break;

		/* Calculate the new indent amount. */
		if (rl == RIGHT)
			newcol = oldcol + sw;
		else {
			newcol = oldcol < sw ? 0 : oldcol - sw;
			if (newcol == oldcol) {
				if (sp->lno == from)
					curset = 1;
				continue;
			}
		}

		/* Get a buffer that will hold the new line. */
		ADD_SPACE_RET(sp, bp, blen, newcol + len);

		/*
		 * Build a new indent string and count the number of
		 * characters it uses.
		 */
		for (tbp = bp, newidx = 0;
		    newcol >= O_VAL(sp, O_TABSTOP); ++newidx) {
			*tbp++ = '\t';
			newcol -= O_VAL(sp, O_TABSTOP);
		}
		for (; newcol > 0; --newcol, ++newidx)
			*tbp++ = ' ';

		/* Add the original line. */
		memmove(tbp, p + oldidx, len - oldidx);

		/* Set the replacement line. */
		if (file_sline(sp, from, bp, (tbp + (len - oldidx)) - bp)) {
err:			FREE_SPACE(sp, bp, blen);
			return (1);
		}

		/*
		 * !!!
		 * The shift command in historic vi had the usual bizarre
		 * collection of cursor semantics.  If called from vi, the
		 * cursor was repositioned to the first non-blank character
		 * of the lowest numbered line shifted.  If called from ex,
		 * the cursor was repositioned to the first non-blank of the
		 * highest numbered line shifted.  Here, if the cursor isn't
		 * part of the set of lines that are moved, move it to the
		 * first non-blank of the last line shifted.  (This makes
		 * ":3>>" in vi work reasonably.)  If the cursor is part of
		 * the shifted lines, it doesn't get moved at all.  This
		 * permits shifting of marked areas, i.e. ">'a." shifts the
		 * marked area twice, something that couldn't be done with
		 * historic vi.
		 */
		if (sp->lno == from) {
			curset = 1;
			if (newidx > oldidx)
				sp->cno += newidx - oldidx;
			else if (sp->cno >= oldidx - newidx)
				sp->cno -= oldidx - newidx;
		}
	}
	if (!curset) {
		sp->lno = to;
		sp->cno = 0;
		(void)nonblank(sp, to, &sp->cno);
	}

	FREE_SPACE(sp, bp, blen);

	sp->rptlines[L_SHIFT] += cmdp->addr2.lno - cmdp->addr1.lno + 1;
	return (0);
}

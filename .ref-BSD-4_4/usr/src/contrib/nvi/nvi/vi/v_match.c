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
static char sccsid[] = "@(#)v_match.c	8.1 (Berkeley) 6/9/93";
#endif /* not lint */

#include <sys/types.h>

#include <string.h>

#include "vi.h"
#include "vcmd.h"

static int	findmatchc __P((MARK *, char *, size_t, MARK *));

/*
 * v_match -- %
 *	Search to matching character.
 */
int
v_match(sp, ep, vp, fm, tm, rp)
	SCR *sp;
	EXF *ep;
	VICMDARG *vp;
	MARK *fm, *tm, *rp;
{
	register int cnt, matchc, startc;
	enum direction dir;
	recno_t lno;
	size_t len;
	int ch;
	char *p;

	if ((p = file_gline(sp, ep, fm->lno, &len)) == NULL) {
		if (file_lline(sp, ep, &lno))
			return (1);
		if (lno == 0)
			goto nomatch;
		GETLINE_ERR(sp, fm->lno);
		return (1);
	}

	if (len == 0)
		goto nomatch;

	switch (startc = p[fm->cno]) {
	case '(':
		matchc = ')';
		dir = FORWARD;
		break;
	case ')':
		matchc = '(';
		dir = BACKWARD;
		break;
	case '[':
		matchc = ']';
		dir = FORWARD;
		break;
	case ']':
		matchc = '[';
		dir = BACKWARD;
		break;
	case '{':
		matchc = '}';
		dir = FORWARD;
		break;
	case '}':
		matchc = '{';
		dir = BACKWARD;
		break;
	default:
		if (F_ISSET(vp, VC_C | VC_D | VC_Y)) {
			msgq(sp, M_BERR,
			"Proximity match doesn't work for motion commands.");
			return (1);
		}
		if (findmatchc(fm, p, len, rp)) {
nomatch:		msgq(sp, M_BERR, "No match character on this line.");
			return (1);
		}
		return (0);
	}

	if (getc_init(sp, ep, fm, &ch))
		return (1);
	for (cnt = 1; getc_next(sp, ep, dir, &ch);)
		if (ch == startc)
			++cnt;
		else if (ch == matchc && --cnt == 0)
			break;
	if (cnt) {
		msgq(sp, M_BERR, "Matching character not found.");
		return (1);
	}
	getc_set(sp, ep, rp);

	/* Movement commands go one space further. */
	if (F_ISSET(vp, VC_C | VC_D | VC_Y)) {
		if (file_gline(sp, ep, rp->lno, &len) == NULL) {
			GETLINE_ERR(sp, rp->lno);
			return (1);
		}
		if (len)
			++rp->cno;
	}
	return (0);
}

/*
 * findmatchc --
 *	If we're not on a character we know how to match, try and find the
 *	closest character from the set "{}[]()".  The historic vi would also
 *	look for a character to match against, but in an inexplicable and
 *	apparently random fashion.  We search forward, then backward, and go
 *	to the closest one.  Ties go left for no reason.
 */
static int
findmatchc(fm, p, len, rp)
	MARK *fm, *rp;
	char *p;
	size_t len;
{
	register size_t off;
	size_t left, right;			/* Can't be uninitialized. */
	int leftfound, rightfound;
	char *t;

	leftfound = rightfound = 0;
	for (off = 0, t = &p[off]; off++ < fm->cno;)
		if (strchr("{}[]()", *t++)) {
			left = off - 1;
			leftfound = 1;
			break;
		}

	--len;
	for (off = fm->cno + 1, t = &p[off]; off++ < len;)
		if (strchr("{}[]()", *t++)) {
			right = off - 1;
			rightfound = 1;
			break;
		}

	rp->lno = fm->lno;
	if (leftfound)
		if (rightfound)
			if (fm->cno - left > right - fm->cno)
				rp->cno = right;
			else
				rp->cno = left;
		else
			rp->cno = left;
	else if (rightfound)
		rp->cno = right;
	else
		return (1);
	return (0);
}

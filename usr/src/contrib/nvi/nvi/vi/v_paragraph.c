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
static char sccsid[] = "@(#)v_paragraph.c	8.1 (Berkeley) 6/9/93";
#endif /* not lint */

#include <sys/types.h>

#include <errno.h>
#include <stdlib.h>
#include <string.h>

#include "vi.h"
#include "vcmd.h"

/*
 * Paragraphs are empty lines after text or values from the paragraph or
 * section options.
 */

/*
 * v_paragraphf -- [count]}
 *	Move forward count paragraphs.
 */
int
v_paragraphf(sp, ep, vp, fm, tm, rp)
	SCR *sp;
	EXF *ep;
	VICMDARG *vp;
	MARK *fm, *tm, *rp;
{
	enum { P_INTEXT, P_INBLANK } pstate;
	size_t lastlen, len;
	recno_t cnt, lastlno, lno;
	char *p, *lp;

	/* Figure out what state we're currently in. */
	lno = fm->lno;
	if ((p = file_gline(sp, ep, lno, &len)) == NULL)
		goto eof;

	/*
	 * If we start in text, we want to switch states 2 * N - 1
	 * times, in non-text, 2 * N times.
	 */
	cnt = F_ISSET(vp, VC_C1SET) ? vp->count : 1;
	cnt *= 2;
	if (len == 0)
		pstate = P_INBLANK;
	else {
		--cnt;
		pstate = P_INTEXT;
	}

	for (;;) {
		lastlno = lno;
		lastlen = len;
		if ((p = file_gline(sp, ep, ++lno, &len)) == NULL)
			goto eof;
		switch (pstate) {
		case P_INTEXT:
			if (p[0] == '.' && len >= 2)
				for (lp = sp->paragraph; *lp; lp += 2)
					if (lp[0] == p[1] &&
					    (lp[1] == ' ' || lp[1] == p[2]) &&
					    !--cnt)
						goto found;
			if (len == 0) {
				if (!--cnt)
					goto found;
				pstate = P_INBLANK;
			}
			break;
		case P_INBLANK:
			if (len == 0)
				break;
			if (--cnt) {
				pstate = P_INTEXT;
				break;
			}
			/*
			 * Historically, a motion command was up to the end
			 * of the previous line, whereas the movement command
			 * was to the start of the new "paragraph".
			 */
found:			if (F_ISSET(vp, VC_C | VC_D | VC_Y)) {
				rp->lno = lastlno;
				rp->cno = lastlen ? lastlen + 1 : 0;
			} else {
				rp->lno = lno;
				rp->cno = 0;
			}
			return (0);
		default:
			abort();
		}
	}

	/*
	 * EOF is a movement sink, however, the } command historically
	 * moved to the end of the last line if repeatedly invoked.
	 */
eof:	if (fm->lno != lno - 1) {
		rp->lno = lno - 1;
		rp->cno = len ? len - 1 : 0;
		return (0);
	}
	if ((p = file_gline(sp, ep, fm->lno, &len)) == NULL)
		GETLINE_ERR(sp, fm->lno);
	if (fm->cno != (len ? len - 1 : 0)) {
		rp->lno = lno - 1;
		rp->cno = len ? len - 1 : 0;
		return (0);
	}
	v_eof(sp, ep, NULL);
	return (1);
}

/*
 * v_paragraphb -- [count]{
 *	Move forward count paragraph.
 */
int
v_paragraphb(sp, ep, vp, fm, tm, rp)
	SCR *sp;
	EXF *ep;
	VICMDARG *vp;
	MARK *fm, *tm, *rp;
{
	enum { P_INTEXT, P_INBLANK } pstate;
	size_t len;
	recno_t cnt, lno;
	char *p, *lp;

	/*
	 * The { command historically moved to the beginning of the first
	 * line if invoked on the first line.
	 *
	 * Check for SOF.
	 */
	if (fm->lno <= 1) {
		if (fm->cno == 0) {
			v_sof(sp, NULL);
			return (1);
		}
		rp->lno = 1;
		rp->cno = 0;
		return (0);
	}

	/* Figure out what state we're currently in. */
	lno = fm->lno;
	if ((p = file_gline(sp, ep, lno, &len)) == NULL)
		goto sof;

	/*
	 * If we start in text, we want to switch states 2 * N - 1
	 * times, in non-text, 2 * N times.
	 */
	cnt = F_ISSET(vp, VC_C1SET) ? vp->count : 1;
	cnt *= 2;
	if (len == 0)
		pstate = P_INBLANK;
	else {
		--cnt;
		pstate = P_INTEXT;
	}

	for (;;) {
		if ((p = file_gline(sp, ep, --lno, &len)) == NULL)
			goto sof;
		switch (pstate) {
		case P_INTEXT:
			if (p[0] == '.' && len >= 2)
				for (lp = sp->paragraph; *lp; lp += 2)
					if (lp[0] == p[1] &&
					    (lp[1] == ' ' || lp[1] == p[2]) &&
					    !--cnt)
						goto found;
			if (len == 0) {
				if (!--cnt)
					goto found;
				pstate = P_INBLANK;
			}
			break;
		case P_INBLANK:
			if (len != 0) {
				if (!--cnt) {
found:					rp->lno = lno;
					rp->cno = 0;
					return (0);
				}
				pstate = P_INTEXT;
			}
			break;
		default:
			abort();
		}
	}

	/* SOF is a movement sink. */
sof:	rp->lno = 1;
	rp->cno = 0;
	return (0);
}

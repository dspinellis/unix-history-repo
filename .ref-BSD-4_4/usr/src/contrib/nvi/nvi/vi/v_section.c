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
static char sccsid[] = "@(#)v_section.c	8.1 (Berkeley) 6/9/93";
#endif /* not lint */

#include <sys/types.h>

#include <string.h>

#include "vi.h"
#include "vcmd.h"

/*
 * In historic vi, the section commands ignored empty lines, unlike the
 * paragraph commands, which was probably okay, but also moved to the
 * start of the last line when there where no more sections instead of
 * the end of the last line.  This has been changed to be more like the
 * paragraphs command.
 *
 * In historic vi, a "function" was defined as the first character on
 * the line being an open brace.
 */

/*
 * v_sectionf -- [count]]]
 *	Move forward count sections/functions.
 */
int
v_sectionf(sp, ep, vp, fm, tm, rp)
	SCR *sp;
	EXF *ep;
	VICMDARG *vp;
	MARK *fm, *tm, *rp;
{
	size_t len;
	recno_t cnt, lno;
	char *p, *list, *lp;

	/* Get macro list. */
	if ((list = O_STR(sp, O_SECTIONS)) == NULL)
		return (1);
	if (strlen(list) & 1) {
		msgq(sp, M_ERR,
		    "Section options must be in groups of two characters.");
		return (1);
	}

	rp->cno = 0;
	cnt = F_ISSET(vp, VC_C1SET) ? vp->count : 1;
	for (lno = fm->lno; p = file_gline(sp, ep, ++lno, &len);)
		switch(len) {
		case 0:
			break;;
		case 1:
			if (p[0] == '{' && !--cnt) {
				rp->lno = lno;
				return (0);
			}
			break;
		default:
			if (p[0] != '.')
				break;
			/* Check for macro. */
			for (lp = list; *lp; lp += 2)
				if (lp[0] == p[1] &&
				    (lp[1] == ' ' || lp[1] == p[2]) && !--cnt) {
					rp->lno = lno;
					return (0);
				}
			break;
		}

	/* EOF is a movement sink. */
	if (fm->lno != lno - 1) {
		rp->lno = lno - 1;
		rp->cno = len ? len - 1 : 0;
		return (0);
	}
	v_eof(sp, ep, NULL);
	return (1);
}

/*
 * v_sectionb -- [count][[
 *	Move backward count sections/functions.
 */
int
v_sectionb(sp, ep, vp, fm, tm, rp)
	SCR *sp;
	EXF *ep;
	VICMDARG *vp;
	MARK *fm, *tm, *rp;
{
	size_t len;
	recno_t cnt, lno;
	char *p, *list, *lp;

	/* Check for SOF. */
	if (fm->lno <= 1) {
		v_sof(sp, NULL);
		return (1);
	}

	if ((list = O_STR(sp, O_SECTIONS)) == NULL)
		return (1);
	if (strlen(list) & 1) {
		msgq(sp, M_ERR,
		    "Section options must be in groups of two characters.");
		return (1);
	}

	rp->cno = 0;
	cnt = F_ISSET(vp, VC_C1SET) ? vp->count : 1;
	for (lno = fm->lno; p = file_gline(sp, ep, --lno, &len);)
		switch(len) {
		case 0:
			break;
		case 1:
			if (p[0] == '{' && !--cnt) {
				rp->lno = lno;
				return (0);
			}
			break;
		default:
			if (p[0] != '.')
				break;
			/* Check for macro. */
			for (lp = list; *lp; lp += 2)
				if (lp[0] == p[1] &&
				    (lp[1] == ' ' || lp[1] == p[2]) && !--cnt) {
					rp->lno = lno;
					return (0);
				}
			break;
		}

	/* SOF is a movement sink. */
	rp->lno = 1;
	return (0);
}

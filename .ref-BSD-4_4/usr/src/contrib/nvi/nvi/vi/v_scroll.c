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
static char sccsid[] = "@(#)v_scroll.c	8.1 (Berkeley) 6/9/93";
#endif /* not lint */

#include <sys/types.h>

#include "vi.h"
#include "vcmd.h"

/*
 * v_lgoto -- [count]G
 *	Go to first non-blank character of the line count, the last line
 *	of the file by default.
 */
int
v_lgoto(sp, ep, vp, fm, tm, rp)
	SCR *sp;
	EXF *ep;
	VICMDARG *vp;
	MARK *fm, *tm, *rp;
{
	recno_t last;

	if (file_lline(sp, ep, &last))
		return (1);
	if (F_ISSET(vp, VC_C1SET)) {
		if (last < vp->count) {
			v_eof(sp, ep, fm);
			return (1);
		}
		rp->lno = vp->count;
	} else
		rp->lno = last ? last : 1;
	return (0);
}

/* 
 * v_home -- [count]H
 *	Move to the first non-blank character of the line count from
 *	the top of the screen, 1 by default.
 */
int
v_home(sp, ep, vp, fm, tm, rp)
	SCR *sp;
	EXF *ep;
	VICMDARG *vp;
	MARK *fm, *tm, *rp;
{
	return (sp->s_position(sp, ep, &rp->lno,
	    F_ISSET(vp, VC_C1SET) ? vp->count : 1, P_TOP));
}

/*
 * v_middle -- M
 *	Move to the first non-blank character of the line in the middle
 *	of the screen.
 */
int
v_middle(sp, ep, vp, fm, tm, rp)
	SCR *sp;
	EXF *ep;
	VICMDARG *vp;
	MARK *fm, *tm, *rp;
{
	return (sp->s_position(sp, ep, &rp->lno, 0, P_MIDDLE));
}

/*
 * v_bottom -- [count]L
 *	Move to the first non-blank character of the line count from
 *	the bottom of the screen, 1 by default.
 */
int
v_bottom(sp, ep, vp, fm, tm, rp)
	SCR *sp;
	EXF *ep;
	VICMDARG *vp;
	MARK *fm, *tm, *rp;
{
	return (sp->s_position(sp, ep, &rp->lno,
	    F_ISSET(vp, VC_C1SET) ? vp->count : 1, P_BOTTOM));
}

/*
 * v_up -- [count]^P, [count]k, [count]-
 *	Move up by lines.
 */
int
v_up(sp, ep, vp, fm, tm, rp)
	SCR *sp;
	EXF *ep;
	VICMDARG *vp;
	MARK *fm, *tm, *rp;
{
	recno_t lno;

	lno = F_ISSET(vp, VC_C1SET) ? vp->count : 1;

	if (fm->lno <= lno) {
		v_sof(sp, fm);
		return (1);
	}
	rp->lno = fm->lno - lno;
	return (0);
}

/*
 * v_down -- [count]^J, [count]^N, [count]j, [count]^M, [count]+
 *	Move down by lines.
 */
int
v_down(sp, ep, vp, fm, tm, rp)
	SCR *sp;
	EXF *ep;
	VICMDARG *vp;
	MARK *fm, *tm, *rp;
{
	recno_t lno;
	size_t len;

	lno = fm->lno + (F_ISSET(vp, VC_C1SET) ? vp->count : 1);

	if (file_gline(sp, ep, lno, &len) == NULL) {
		v_eof(sp, ep, fm);
		return (1);
	}
	rp->lno = lno;
	rp->cno = len ? fm->cno > len - 1 ? len - 1 : fm->cno : 0;
	return (0);
}

/*
 * The historic vi had a problem in that all movements were by physical
 * lines, not by logical, or screen lines.  Arguments can be made that this
 * is the right thing to do.  For example, single line movements, such as
 * 'j' or 'k', should probably work on physical lines.  Commands like "dj",
 * or "j.", where '.' is a change command, make more sense for physical lines
 * than they do logical lines.  The arguments, however, don't apply to
 * scrolling commands like ^D and ^F -- if the window is fairly small, using
 * physical lines can result in a half-page scroll repainting the entire
 * screen, which is not what the user wanted.  In addition, if the line is
 * large and the screen is small, using physical lines can make it impossible
 * to display parts of the line.  This implementation does the scrolling
 * (^B, ^D, ^F, ^U), ^Y and ^E commands using logical lines, not physical.
 *
 * Another minor issue is that historically, page and half-page scrolling
 * commands moved to the first non-blank character in the new line.  If
 * the line changes because of the scroll, we do that as well, but if the
 * line doesn't change because the line is so large that the scroll happened
 * inside of the line, we leave the cursor alone.
 */

/*
 * v_hpageup -- [count]^U
 *	Page up half screens.
 */
int
v_hpageup(sp, ep, vp, fm, tm, rp)
	SCR *sp;
	EXF *ep;
	VICMDARG *vp;
	MARK *fm, *tm, *rp;
{
	/* 
	 * Half screens always succeed unless already at SOF.  Half screens
	 * set the scroll value, even if the command ultimately failed, in
	 * historic vi.  It's probably a don't care.
	 */
	if (F_ISSET(vp, VC_C1SET))
		O_VAL(sp, O_SCROLL) = vp->count;
	else
		vp->count = O_VAL(sp, O_SCROLL);

	if (sp->s_down(sp, ep, rp, (recno_t)O_VAL(sp, O_SCROLL), 1))
		return (1);

	if (rp->lno != fm->lno && nonblank(sp, ep, rp->lno, &rp->cno))
		return (1);
	return (0);
}

/*
 * v_hpagedown -- [count]^D
 *	Page down half screens.
 */
int
v_hpagedown(sp, ep, vp, fm, tm, rp)
	SCR *sp;
	EXF *ep;
	VICMDARG *vp;
	MARK *fm, *tm, *rp;
{
	/* 
	 * Half screens always succeed unless already at EOF.  Half screens
	 * set the scroll value, even if the command ultimately failed, in
	 * historic vi.  It's probably a don't care.
	 */
	if (F_ISSET(vp, VC_C1SET))
		O_VAL(sp, O_SCROLL) = vp->count;
	else
		vp->count = O_VAL(sp, O_SCROLL);

	if (sp->s_up(sp, ep, rp, (recno_t)O_VAL(sp, O_SCROLL), 1))
		return (1);

	if (rp->lno != fm->lno && nonblank(sp, ep, rp->lno, &rp->cno))
		return (1);
	return (0);
}

/*
 * v_pageup -- [count]^B
 *	Page up full screens.
 */
int
v_pageup(sp, ep, vp, fm, tm, rp)
	SCR *sp;
	EXF *ep;
	VICMDARG *vp;
	MARK *fm, *tm, *rp;
{
	recno_t count;

	/* Calculation from POSIX 1003.2/D8. */
	count = (F_ISSET(vp, VC_C1SET) ? vp->count : 1) * (sp->t_rows - 1);

	if (sp->s_down(sp, ep, rp, count, 1))
		return (1);

	if (rp->lno != fm->lno && nonblank(sp, ep, rp->lno, &rp->cno))
		return (1);
	return (0);
}

/*
 * v_pagedown -- [count]^F
 *	Page down full screens.
 */
int
v_pagedown(sp, ep, vp, fm, tm, rp)
	SCR *sp;
	EXF *ep;
	VICMDARG *vp;
	MARK *fm, *tm, *rp;
{
	recno_t count;

	/* Calculation from POSIX 1003.2/D8. */
	count = (F_ISSET(vp, VC_C1SET) ? vp->count : 1) * (sp->t_rows - 1);

	if (sp->s_up(sp, ep, rp, count, 1))
		return (1);

	if (rp->lno != fm->lno && nonblank(sp, ep, rp->lno, &rp->cno))
		return (1);
	return (0);
}

/*
 * v_lineup -- [count]^Y
 *	Page up by lines.
 */
int
v_lineup(sp, ep, vp, fm, tm, rp)
	SCR *sp;
	EXF *ep;
	VICMDARG *vp;
	MARK *fm, *tm, *rp;
{
	/*
	 * The cursor moves down, staying with its original line, unless it
	 * reaches the bottom of the screen.
	 */
	return (sp->s_down(sp, ep,
	    rp, F_ISSET(vp, VC_C1SET) ? vp->count : 1, 0));
}

/*
 * v_linedown -- [count]^E
 *	Page down by lines.
 */
int
v_linedown(sp, ep, vp, fm, tm, rp)
	SCR *sp;
	EXF *ep;
	VICMDARG *vp;
	MARK *fm, *tm, *rp;
{
	/*
	 * The cursor moves up, staying with its original line, unless it
	 * reaches the top of the screen.
	 */
	return (sp->s_up(sp, ep,
	    rp, F_ISSET(vp, VC_C1SET) ? vp->count : 1, 0));
}

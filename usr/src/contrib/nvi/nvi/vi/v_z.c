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
static char sccsid[] = "@(#)v_z.c	8.1 (Berkeley) 6/9/93";
#endif /* not lint */

#include <sys/types.h>

#include "vi.h"
#include "vcmd.h"

/*
 * v_z -- [count]z[count][.-<CR>]
 *	Move the screen.
 */
int
v_z(sp, ep, vp, fm, tm, rp)
	SCR *sp;
	EXF *ep;
	VICMDARG *vp;
	MARK *fm, *tm, *rp;
{
	recno_t last, lno;

	/*
	 * The first count is the line to use.  If the value doesn't
	 * exist, use the last line.
	 */
	if (F_ISSET(vp, VC_C1SET)) {
		lno = vp->count;
		if (file_lline(sp, ep, &last))
			return (1);
		if (lno > last)
			lno = last;
	} else
		lno = fm->lno;

	/* The second count is the window size. */
	if (F_ISSET(vp, VC_C2SET) && set_window_size(sp, vp->count2))
		return (1);

	switch(vp->character) {
	case '.':
		if (sp->s_fill(sp, ep, lno, P_MIDDLE))
			return (1);
		break;
	case '-':
		if (sp->s_fill(sp, ep, lno, P_BOTTOM))
			return (1);
		break;
	default:
		if (sp->special[vp->character] == K_CR) {
			if (sp->s_fill(sp, ep, lno, P_TOP))
				return (1);
			break;
		}
		msgq(sp, M_ERR, "usage: %s.", vp->kp->usage);
		return (1);
	}

	/* If the map changes, have to redraw the entire screen. */
	F_SET(sp, S_REDRAW);

	rp->lno = lno;
	rp->cno = fm->cno;
	return (0);
}

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
static char sccsid[] = "@(#)svi_get.c	8.1 (Berkeley) 6/9/93";
#endif /* not lint */

#include <sys/types.h>

#include <ctype.h>
#include <curses.h>
#include <errno.h>
#include <stdlib.h>

#include "vi.h"
#include "vcmd.h"
#include "svi_screen.h"

/*
 * svi_get --
 *	Fill a buffer from the terminal for vi.  The approach is that we
 *	fake like the user is doing input on the last line of the screen.
 *	This makes all of the scrolling work correctly, and allows us the
 *	use of the vi text editing routines, not to mention essentially
 *	infinite length ex commands.
 */
int
svi_get(sp, ep, hp, prompt, flags)
	SCR *sp;
	EXF *ep;
	HDR *hp;
	int prompt;
	u_int flags;
{
	MARK save;
	recno_t bot_lno, top_lno;
	size_t bot_off, top_off;
	int eval;

	/* Save where we are. */
	bot_lno = TMAP->lno;
	bot_off = TMAP->off;
	top_lno = HMAP->lno;
	top_off = HMAP->off;
	save.lno = sp->lno;
	save.cno = sp->cno;

	/* Fake it. */
	sp->t_smap[1].lno = sp->t_smap[0].lno + 1;
	sp->t_smap[1].off = 1;
	sp->lno = sp->t_smap[0].lno + 1;
	sp->cno = 0;
	++sp->t_smap;
	++sp->t_rows;

	/*
	 * Historic vi allowed you to use abbreviations on the command line.
	 * To implement this, add TXT_MAPINPUT to the flags for the v_ntext()
	 * routine.  This is necessary but not sufficient -- v_ntext() will
	 * have to be modified to make available a copy of the original line
	 * without abbreviations, or the :unabbreviate command won't work.
	 */
	eval = v_ntext(sp, ep, &sp->bhdr, NULL, NULL,
	    0, NULL, prompt, 0, flags | TXT_APPENDEOL | TXT_CR | TXT_ESCAPE);

	/* Put it all back. */
	--sp->t_rows;
	--sp->t_smap;
	sp->lno = save.lno;
	sp->cno = save.cno;

	/*
	 * The map may be wrong if the user entered more than one
	 * (logical) line.  Fix it.  If the user entered a whole
	 * screen, this will be slow, but it's not worth caring.
	 */
	while (bot_lno != TMAP->lno || bot_off != TMAP->off)
		if (svi_sm_1down(sp, ep))
			return (1);

	if (svi_refresh(sp, ep))
		return (1);

	return (eval);
}

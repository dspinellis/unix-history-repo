/*-
 * Copyright (c) 1993
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
static char sccsid[] = "@(#)svi_screen.c	8.1 (Berkeley) 6/12/93";
#endif /* not lint */

#include <sys/types.h>

#include <curses.h>
#include <errno.h>
#include <stdlib.h>
#include <string.h>
#include <unistd.h>

#include "vi.h"
#include "vcmd.h"
#include "recover.h"
#include "svi_screen.h"

static int	 screen_end __P((size_t));
static int	 screen_init __P((SCR *));
static SCR	*svi_next __P((SCR *));

/*
 * We use a single curses window for vi.  The model would be simpler with
 * two windows (one for the text, and one for the modeline) because
 * scrolling the text window down would work correctly then, not affecting
 * the mode line.  As it is we have to play games to make it look right.
 * The reason for this choice is that it would be difficult for curses to
 * optimize the movement, i.e. detect that the downward scroll isn't going
 * to change the modeline, set the scrolling region on the terminal and only
 * scroll the first part of the text window.  (Even if curses did detect it,
 * the set-scrolling-region terminal commands can't be used by curses because
 * it's indeterminate where the cursor ends up after they are sent.)
 */
/*
 * svi --
 *	Main vi curses screen loop.
 */
int
svi(sp, ep, spp)
	SCR *sp, **spp;
	EXF *ep;
{
	SCR *np;
	size_t saved_row;

	if (svi_init(sp))
		return (1);
	do {
		if (screen_init(sp))
			return (1);
		F_CLR(sp, S_RESIZE);		/* XXX block SIGWINCH */

		do {
			F_SET(sp, S_CUR_INVALID);
			if (vi(sp, sp->ep)) {
				F_SET(sp, S_EXIT_FORCE);
				if (F_ISSET(ep, F_RCV_ON)) {
					F_SET(ep, F_RCV_NORM);
					(void)rcv_sync(sp, sp->ep);
				}
			}

			saved_row = INFOLINE(sp);

			switch (F_ISSET(sp, S_MAJOR_CHANGE)) {
			case S_EXIT:
				if (file_stop(sp, sp->ep, 0))
					F_CLR(sp, S_EXIT);
				else
					goto noscreen;
				break;
			case S_EXIT_FORCE:
				if (file_stop(sp, sp->ep, 1))
					F_CLR(sp, S_EXIT_FORCE);
				else
					goto noscreen;
				break;
			case S_FSWITCH_FORCE:
				F_CLR(sp, S_FSWITCH_FORCE);
				if (file_stop(sp, sp->ep, 1))
					break;
				if (sp->enext->refcnt == 0 &&
				    file_start(sp, sp->enext, NULL) == NULL)
					goto noscreen;
				sp->eprev = sp->ep;
				sp->ep = sp->enext;
				if (sp != NULL)
					F_SET(sp, S_REFORMAT);
				break;
			case S_FSWITCH:
				F_CLR(sp, S_FSWITCH);
				if (file_stop(sp, sp->ep, 0))
					break;
				if (sp->enext->refcnt == 0 &&
				    file_start(sp, sp->enext, NULL) == NULL) {
noscreen:				np = svi_next(sp);
					if (scr_end(sp))
						return (1);
					sp = np;
				} else {
					sp->eprev = sp->ep;
					sp->ep = sp->enext;
				}
				if (sp != NULL)
					F_SET(sp, S_REFORMAT);
				break;
			case S_SSWITCH:
				F_CLR(sp, S_SSWITCH);
				sp = sp->snext;
				break;
			case 0:
				break;
			default:
				abort();
			}
		} while (sp != NULL && F_ISSET(sp, S_MODE_VI) &&
		    !F_ISSET(sp, S_EXIT | S_EXIT_FORCE | S_RESIZE));

		if (screen_end(saved_row))
			return (1);
	} while (sp != NULL && F_ISSET(sp, S_MODE_VI) &&
	    !F_ISSET(sp, S_EXIT | S_EXIT_FORCE));
	*spp = sp;
	return (0);
}

/*
 * svi_init --
 *	Initialize a screen.
 */
int
svi_init(sp)
	SCR *sp;
{
	/* Initialize support routines. */
	sp->s_bell		= svi_bell;
	sp->s_busy_cursor	= svi_busy_cursor;
	sp->s_change		= svi_change;
	sp->s_chposition	= svi_chposition;
	sp->s_confirm		= svi_confirm;
	sp->s_down		= svi_sm_down;
	sp->s_ex_cmd		= svi_ex_cmd;
	sp->s_ex_run		= svi_ex_run;
	sp->s_ex_write		= svi_ex_write;
	sp->s_fill		= svi_sm_fill;
	sp->s_get		= svi_get;
	sp->s_position		= svi_sm_position;
	sp->s_refresh		= svi_refresh;
	sp->s_relative		= svi_relative;
	sp->s_split		= svi_split;
	sp->s_suspend		= svi_suspend;
	sp->s_up		= svi_sm_up;
	return (0);
}

/*
 * screen_init --
 *	Initialize curses, the screen structure.
 */
static int
screen_init(sp)
	SCR *sp;
{
	if (initscr() == NULL) {	/* Start the screen. */
		msgq(sp, M_ERR, "Error: initscr failed: %s", strerror(errno));
		return (1);
	}
	raw();				/* No special characters. */
	noecho();			/* No character echo. */
	idlok(stdscr, 1);		/* Use hardware scrolling. */

	/* Initialize line/cols values. */
	sp->rows = sp->w_rows = LINES;
	sp->cols = COLS;		/* LINES, COLS: XXX */
	sp->sc_row = sp->sc_col = 0;
	sp->t_rows = LINES - 1;

	/* Create the screen map. */
	if ((sp->h_smap = malloc(sp->w_rows * sizeof(SMAP))) == NULL)
		return (1);
	sp->t_smap = sp->h_smap + (sp->t_rows - 1);

	/* Turn off any optimizations. */
	sp->olno = OOBLNO;

	return (0);
}

/*
 * screen_end --
 *	Move to the bottom of the screen, end curses.
 */
static int
screen_end(row)
	size_t row;
{
	if (move(row, 0) == OK) {
		clrtoeol();
		refresh();
	}
	endwin();
	return (0);
}

/*
 * svi_next --
 *	Find a related screen to edit.
 */
static SCR *
svi_next(sp)
	SCR *sp;
{
	SCR *p;

	/* If a split screen, add space to parent/child. */
	if ((p = sp->parent) != NULL) {
		p->rows += sp->rows;
		p->t_rows += sp->t_rows + 1;
		p->t_smap = p->h_smap + (p->t_rows - 1);
		if (svi_sm_fill(p, p->ep, p->lno, P_FILL))
			return (NULL);

		F_SET(p, S_REDRAW);
		return (p);
	}
	if ((p = sp->child) != NULL) {
		p->rows += sp->rows;
		p->t_rows += sp->t_rows + 1;
		p->t_smap = p->h_smap + (p->t_rows - 1);
		p->s_off = sp->s_off;
		if (svi_sm_fill(p, p->ep, p->lno, P_FILL))
			return (NULL);

		F_SET(p, S_REDRAW);
		return (p);
	}
	return (NULL);
}

/*
 * Copyright (c) 1981 Regents of the University of California.
 * All rights reserved.
 *
 * %sccs.include.redist.c%
 */

#ifndef lint
static char sccsid[] = "@(#)box.c	5.7 (Berkeley) %G%";
#endif	/* not lint */

#include <curses.h>

/*
 * box --
 *	Draw a box around the given window with "vert" as the vertical
 *	delimiting char, and "hor", as the horizontal one.
 */
int
box(win, vert, hor)
	register WINDOW *win;
	int vert, hor;
{
	register int endy, endx, i;
	register char *fp, *lp, *sfp, *slp;

	endx = win->maxx;
	endy = win->maxy - 1;
	fp = win->lines[0]->line;
	lp = win->lines[endy]->line;
	sfp = win->lines[0]->standout;
	slp = win->lines[endy]->standout;
	for (i = 0; i < endx; i++) {
		fp[i] = lp[i] = hor;
		sfp[i] &= ~__STANDOUT;
		slp[i] &= ~__STANDOUT;
	}
	endx--;
	for (i = 0; i <= endy; i++) {
		win->lines[i]->line[0] = (win->lines[i]->line[endx] = vert);
		win->lines[i]->standout[0] &= ~__STANDOUT;
		win->lines[i]->standout[endx] &= ~__STANDOUT;
	}
	if (!(win->flags & __SCROLLOK) && (win->flags & __SCROLLWIN)) {
		fp[0] = fp[endx] = lp[0] = lp[endx] = ' ';
		sfp[0] &= ~__STANDOUT;
		sfp[endx] &= ~__STANDOUT;
		slp[0] &= ~__STANDOUT;
		slp[endx] &= ~__STANDOUT;
	}
	touchwin(win);
	return (OK);
}

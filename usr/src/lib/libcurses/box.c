/*
 * Copyright (c) 1981 Regents of the University of California.
 * All rights reserved.
 *
 * %sccs.include.redist.c%
 */

#ifndef lint
static char sccsid[] = "@(#)box.c	5.10 (Berkeley) %G%";
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
	register __LDATA *fp, *lp;

	endx = win->maxx;
	endy = win->maxy - 1;
	fp = win->lines[0]->line;
	lp = win->lines[endy]->line;
	for (i = 0; i < endx; i++) {
		fp[i].ch = lp[i].ch = hor;
		fp[i].attr &= ~__STANDOUT;
		lp[i].attr &= ~__STANDOUT;
	}
	endx--;
	for (i = 0; i <= endy; i++) {
		win->lines[i]->line[0].ch = vert;
	        win->lines[i]->line[endx].ch = vert;
		win->lines[i]->line[0].attr &= ~__STANDOUT;
		win->lines[i]->line[endx].attr &= ~__STANDOUT;
	}
	if (!(win->flags & __SCROLLOK) && (win->flags & __SCROLLWIN)) {
		fp[0].ch = fp[endx].ch = lp[0].ch = lp[endx].ch = ' ';
		fp[0].attr &= ~__STANDOUT;
		fp[endx].attr &= ~__STANDOUT;
		lp[0].attr &= ~__STANDOUT;
		lp[endx].attr &= ~__STANDOUT;
	}
	__touchwin(win);
	return (CURSES_OK);
}

/*
 * Copyright (c) 1981 Regents of the University of California.
 * All rights reserved.
 *
 * %sccs.include.redist.c%
 */

#ifndef lint
static char sccsid[] = "@(#)box.c	5.6 (Berkeley) %G%";
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
	register char *fp, *lp;

	endx = win->maxx;
	endy = win->maxy - 1;
	fp = win->topline->line;
	lp = win->lines[endy]->line;
	for (i = 0; i < endx; i++)
		fp[i] = lp[i] = hor;
	endx--;
	for (i = 0; i <= endy; i++)
		win->lines[i]->line[0] = (win->lines[i]->line[endx] = vert);
	if (!(win->flags & __SCROLLOK) && (win->flags & __SCROLLWIN))
		fp[0] = fp[endx] = lp[0] = lp[endx] = ' ';
	touchwin(win);
	return (OK);
}

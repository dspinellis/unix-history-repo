/*
 * Copyright (c) 1981 Regents of the University of California.
 * All rights reserved.
 *
 * %sccs.include.redist.c%
 */

#ifndef lint
static char sccsid[] = "@(#)touchwin.c	5.6 (Berkeley) %G%";
#endif /* not lint */

#include <curses.h>

/*
 * touchwin --
 *	Make it look like the whole window has been changed.
 */
int
touchwin(win)
	register WINDOW *win;
{
	register int y, maxy;

#ifdef DEBUG
	__TRACE("touchwin: (%0.2o)\n", win);
#endif
	maxy = win->maxy;
	for (y = 0; y < maxy; y++)
		touchline(win, y, 0, win->maxx - 1);
	return (OK);
}

/*
 * touchline --
 *	Touch a given line.
 */
int
touchline(win, y, sx, ex)
	register WINDOW *win;
	register int y, sx, ex;
{
#ifdef DEBUG
	__TRACE("touchline: (%0.2o, %d, %d, %d)\n", win, y, sx, ex);
	__TRACE("touchline: first = %d, last = %d\n",
	    win->lines[y]->firstch, win->lines[y]->lastch);
#endif
	sx += win->ch_off;
	ex += win->ch_off;
	if (!(win->lines[y]->flags & __ISDIRTY)) {
		win->lines[y]->flags |= __ISDIRTY;
		win->lines[y]->firstch = sx;
		win->lines[y]->lastch = ex;
	} else {
		if (win->lines[y]->firstch > sx)
			win->lines[y]->firstch = sx;
		if (win->lines[y]->lastch < ex)
			win->lines[y]->lastch = ex;
	}
#ifdef DEBUG
	__TRACE("touchline: first = %d, last = %d\n",
	    win->lines[y]->firstch, win->lines[y]->lastch);
#endif
	return (OK);
}

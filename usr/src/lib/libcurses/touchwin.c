/*
 * Copyright (c) 1981 Regents of the University of California.
 * All rights reserved.
 *
 * %sccs.include.redist.c%
 */

#ifndef lint
static char sccsid[] = "@(#)touchwin.c	5.5 (Berkeley) %G%";
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
	maxy = win->_maxy;
	for (y = 0; y < maxy; y++)
		touchline(win, y, 0, win->_maxx - 1);
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
	    win->_firstch[y], win->_lastch[y]);
#endif
	sx += win->_ch_off;
	ex += win->_ch_off;
	if (win->_firstch[y] == _NOCHANGE) {
		win->_firstch[y] = sx;
		win->_lastch[y] = ex;
	} else {
		if (win->_firstch[y] > sx)
			win->_firstch[y] = sx;
		if (win->_lastch[y] < ex)
			win->_lastch[y] = ex;
	}
#ifdef DEBUG
	__TRACE("touchline: first = %d, last = %d\n",
	    win->_firstch[y], win->_lastch[y]);
#endif
	return (OK);
}

/*
 * Copyright (c) 1981 Regents of the University of California.
 * All rights reserved.
 *
 * %sccs.include.redist.c%
 */

#ifndef lint
static char sccsid[] = "@(#)erase.c	5.5 (Berkeley) %G%";
#endif	/* not lint */

#include <curses.h>

/*
 * werase --
 *	Erases everything on the window.
 */
int
werase(win)
	register WINDOW *win;
{

	register int minx, y;
	register char *sp, *end, *start, *maxx;

#ifdef DEBUG
	__TRACE("werase: (%0.2o)\n", win);
#endif
	for (y = 0; y < win->_maxy; y++) {
		minx = _NOCHANGE;
		start = win->_y[y];
		end = &start[win->_maxx];
		for (sp = start; sp < end; sp++)
			if (*sp != ' ') {
				maxx = sp;
				if (minx == _NOCHANGE)
					minx = sp - start;
				*sp = ' ';
			}
		if (minx != _NOCHANGE)
			touchline(win, y, minx, maxx - win->_y[y]);
	}
	win->_curx = win->_cury = 0;
	return (OK);
}

/*
 * Copyright (c) 1981 Regents of the University of California.
 * All rights reserved.
 *
 * %sccs.include.redist.c%
 */

#ifndef lint
static char sccsid[] = "@(#)mvwin.c	5.5 (Berkeley) %G%";
#endif	/* not lint */

#include <curses.h>

/*
 * mvwin --
 *	Relocate the starting position of a window.
 */
int
mvwin(win, by, bx)
	register WINDOW *win;
	register int by, bx;
{
	register WINDOW *orig;
	register int dy, dx;

	if (by + win->_maxy > LINES || bx + win->_maxx > COLS)
		return (ERR);
	dy = by - win->_begy;
	dx = bx - win->_begx;
	orig = win->_orig;
	if (orig == NULL) {
		orig = win;
		do {
			win->_begy += dy;
			win->_begx += dx;
			__swflags(win);
			win = win->_nextp;
		} while (win != orig);
	} else {
		if (by < orig->_begy || win->_maxy + dy > orig->_maxy)
			return (ERR);
		if (bx < orig->_begx || win->_maxx + dx > orig->_maxx)
			return (ERR);
		win->_begy = by;
		win->_begx = bx;
		__swflags(win);
		__set_subwin(orig, win);
	}
	touchwin(win);
	return (OK);
}

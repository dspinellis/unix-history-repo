/*
 * Copyright (c) 1981 Regents of the University of California.
 * All rights reserved.
 *
 * %sccs.include.redist.c%
 */

#ifndef lint
static char sccsid[] = "@(#)mvwin.c	5.8 (Berkeley) %G%";
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

	if (by + win->maxy > LINES || bx + win->maxx > COLS)
		return (CURSES_ERR);
	dy = by - win->begy;
	dx = bx - win->begx;
	orig = win->orig;
	if (orig == NULL) {
		orig = win;
		do {
			win->begy += dy;
			win->begx += dx;
			__swflags(win);
			win = win->nextp;
		} while (win != orig);
	} else {
		if (by < orig->begy || win->maxy + dy > orig->maxy)
			return (CURSES_ERR);
		if (bx < orig->begx || win->maxx + dx > orig->maxx)
			return (CURSES_ERR);
		win->begy = by;
		win->begx = bx;
		__swflags(win);
		__set_subwin(orig, win);
	}
	__touchwin(win);
	return (CURSES_OK);
}

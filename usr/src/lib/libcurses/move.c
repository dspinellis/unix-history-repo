/*
 * Copyright (c) 1981 Regents of the University of California.
 * All rights reserved.
 *
 * %sccs.include.redist.c%
 */

#ifndef lint
static char sccsid[] = "@(#)move.c	5.6 (Berkeley) %G%";
#endif	/* not lint */

#include <curses.h>

/*
 * wmove --
 *	Moves the cursor to the given point.
 */
int
wmove(win, y, x)
	register WINDOW *win;
	register int y, x;
{

#ifdef DEBUG
	__TRACE("wmove: (%d, %d)\n", y, x);
#endif
	if (x < 0 || y < 0)
		return (ERR);
	if (x >= win->_maxx || y >= win->_maxy)
		return (ERR);
	win->_curx = x;
	win->_cury = y;
	return (OK);
}

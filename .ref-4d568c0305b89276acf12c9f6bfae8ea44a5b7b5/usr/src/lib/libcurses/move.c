/*
 * Copyright (c) 1981, 1993
 *	The Regents of the University of California.  All rights reserved.
 *
 * %sccs.include.redist.c%
 */

#ifndef lint
static char sccsid[] = "@(#)move.c	8.1 (Berkeley) %G%";
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
	__CTRACE("wmove: (%d, %d)\n", y, x);
#endif
	if (x < 0 || y < 0)
		return (ERR);
	if (x >= win->maxx || y >= win->maxy)
		return (ERR);
	win->curx = x;
	win->lines[win->cury]->flags &= ~__ISPASTEOL;
	win->cury = y;
	win->lines[y]->flags &= ~__ISPASTEOL;
	return (OK);
}

/*
 * Copyright (c) 1981 Regents of the University of California.
 * All rights reserved.
 *
 * %sccs.include.redist.c%
 */

#ifndef lint
static char sccsid[] = "@(#)overlay.c	5.7 (Berkeley) %G%";
#endif	/* not lint */

#include <ctype.h>
#include <curses.h>

/*
 * overlay --
 *	Writes win1 on win2 non-destructively.
 */
int
overlay(win1, win2)
	register WINDOW *win1, *win2;
{

	register int x, y, y1, y2, endy, endx, starty, startx;
	register char *sp, *end;

#ifdef DEBUG
	__TRACE("overlay: (%0.2o, %0.2o);\n", win1, win2);
#endif
	starty = max(win1->_begy, win2->_begy);
	startx = max(win1->_begx, win2->_begx);
	endy = min(win1->_maxy + win1->_begy, win2->_maxy + win2->_begx);
	endx = min(win1->_maxx + win1->_begx, win2->_maxx + win2->_begx);
#ifdef DEBUG
	__TRACE("overlay: from (%d,%d) to (%d,%d)\n",
	    starty, startx, endy, endx);
#endif
	if (starty >= endy || startx >= endx)
		return (OK);
	y1 = starty - win1->_begy;
	y2 = starty - win2->_begy;
	for (y = starty; y < endy; y++, y1++, y2++) {
		end = &win1->_y[y1][endx - win1->_begx];
		x = startx - win2->_begx;
		for (sp = &win1->_y[y1][startx - win1->_begx]; sp < end; sp++) {
			if (!isspace(*sp))
				mvwaddch(win2, y2, x, *sp);
			x++;
		}
	}
	return (OK);
}

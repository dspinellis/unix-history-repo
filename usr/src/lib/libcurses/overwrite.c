/*
 * Copyright (c) 1981 Regents of the University of California.
 * All rights reserved.
 *
 * %sccs.include.redist.c%
 */

#ifndef lint
static char sccsid[] = "@(#)overwrite.c	5.5 (Berkeley) %G%";
#endif	/* not lint */

#include <ctype.h>
#include <curses.h>

/*
 * overwrite --
 *	Writes win1 on win2 destructively.
 */
int
overwrite(win1, win2)
	register WINDOW *win1, *win2;
{
	register int x, y, endy, endx, starty, startx;
	register char *sp, *end;

#ifdef DEBUG
	__TRACE("overwrite: (%0.2o, %0.2o);\n", win1, win2);
#endif
	starty = max(win1->_begy, win2->_begy);
	startx = max(win1->_begx, win2->_begx);
	endy = min(win1->_maxy + win1->_begy, win2->_maxy + win2->_begx);
	endx = min(win1->_maxx + win1->_begx, win2->_maxx + win2->_begx);
	if (starty >= endy || startx >= endx)
		return (OK);
#ifdef DEBUG
	__TRACE("overwrite: from (%d, %d) to (%d, %d)\n",
	    starty, startx, endy, endx);
#endif
	x = endx - startx;
	for (y = starty; y < endy; y++) {
		bcopy(&win1->_y[y - win1->_begy][startx - win1->_begx],
		    &win2->_y[y - win2->_begy][startx - win2->_begx], x);
		touchline(win2, y, startx - win2->_begx, endx - win2->_begx);
	}
	return (OK);
}

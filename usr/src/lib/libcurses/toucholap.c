/*
 * Copyright (c) 1981 Regents of the University of California.
 * All rights reserved.
 *
 * %sccs.include.redist.c%
 */

#ifndef lint
static char sccsid[] = "@(#)toucholap.c	5.5 (Berkeley) %G%";
#endif /* not lint */

#include <curses.h>

/*
 * touchoverlap --
 *	Touch, on win2, the part that overlaps with win1.
 */
int
touchoverlap(win1, win2)
	register WINDOW *win1, *win2;
{
	register int x, y, endy, endx, starty, startx;

#ifdef DEBUG
	__TRACE("touchoverlap: (%0.2o, %0.2o);\n", win1, win2);
#endif
	starty = max(win1->_begy, win2->_begy);
	startx = max(win1->_begx, win2->_begx);
	endy = min(win1->_maxy + win1->_begy, win2->_maxy + win2->_begx);
	endx = min(win1->_maxx + win1->_begx, win2->_maxx + win2->_begx);
#ifdef DEBUG
	__TRACE("touchoverlap: from (%d,%d) to (%d,%d)\n",
	    starty, startx, endy, endx);
	__TRACE("touchoverlap: win1 (%d,%d) to (%d,%d)\n",
	    win1->_begy, win1->_begx, win1->_begy + win1->_maxy,
	    win1->_begx + win1->_maxx);
	__TRACE("touchoverlap: win2 (%d,%d) to (%d,%d)\n",
	    win2->_begy, win2->_begx, win2->_begy + win2->_maxy,
	    win2->_begx + win2->_maxx);
#endif
	if (starty >= endy || startx >= endx)
		return (OK);
	starty -= win2->_begy;
	startx -= win2->_begx;
	endy -= win2->_begy;
	endx -= win2->_begx;
	for (--endx, y = starty; y < endy; y++)
		touchline(win2, y, startx, endx);
	return (OK);
}

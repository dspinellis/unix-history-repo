/*
 * Copyright (c) 1981 Regents of the University of California.
 * All rights reserved.
 *
 * %sccs.include.redist.c%
 */

#ifndef lint
static char sccsid[] = "@(#)toucholap.c	5.10 (Berkeley) %G%";
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
	register int y, endy, endx, starty, startx;

#ifdef DEBUG
	__TRACE("touchoverlap: (%0.2o, %0.2o);\n", win1, win2);
#endif
	starty = max(win1->begy, win2->begy);
	startx = max(win1->begx, win2->begx);
	endy = min(win1->maxy + win1->begy, win2->maxy + win2->begx);
	endx = min(win1->maxx + win1->begx, win2->maxx + win2->begx);
#ifdef DEBUG
	__TRACE("touchoverlap: from (%d,%d) to (%d,%d)\n",
	    starty, startx, endy, endx);
	__TRACE("touchoverlap: win1 (%d,%d) to (%d,%d)\n",
	    win1->begy, win1->begx, win1->begy + win1->maxy,
	    win1->begx + win1->maxx);
	__TRACE("touchoverlap: win2 (%d,%d) to (%d,%d)\n",
	    win2->begy, win2->begx, win2->begy + win2->maxy,
	    win2->begx + win2->maxx);
#endif
	if (starty >= endy || startx >= endx)
		return (OK);
	starty -= win2->begy;
	startx -= win2->begx;
	endy -= win2->begy;
	endx -= win2->begx;
	for (--endx, y = starty; y < endy; y++)
		__touchline(win2, y, startx, endx, 0);
	return (OK);
}


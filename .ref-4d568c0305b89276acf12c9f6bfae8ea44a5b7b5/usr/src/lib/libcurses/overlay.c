/*
 * Copyright (c) 1981, 1993
 *	The Regents of the University of California.  All rights reserved.
 *
 * %sccs.include.redist.c%
 */

#ifndef lint
static char sccsid[] = "@(#)overlay.c	8.1 (Berkeley) %G%";
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
	register __LDATA *sp, *end;

#ifdef DEBUG
	__CTRACE("overlay: (%0.2o, %0.2o);\n", win1, win2);
#endif
	starty = max(win1->begy, win2->begy);
	startx = max(win1->begx, win2->begx);
	endy = min(win1->maxy + win1->begy, win2->maxy + win2->begx);
	endx = min(win1->maxx + win1->begx, win2->maxx + win2->begx);
#ifdef DEBUG
	__CTRACE("overlay: from (%d,%d) to (%d,%d)\n",
	    starty, startx, endy, endx);
#endif
	if (starty >= endy || startx >= endx)
		return (OK);
	y1 = starty - win1->begy;
	y2 = starty - win2->begy;
	for (y = starty; y < endy; y++, y1++, y2++) {
		end = &win1->lines[y1]->line[endx - win1->begx];
		x = startx - win2->begx;
		for (sp = &win1->lines[y1]->line[startx - win1->begx]; 
		     sp < end; sp++) {
			if (!isspace(sp->ch)) {
				wmove(win2, y2, x);
				__waddch(win2, sp);
			}
			x++;
		}
	}
	return (OK);
}

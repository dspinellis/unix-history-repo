/*
 * Copyright (c) 1981, 1993
 *	The Regents of the University of California.  All rights reserved.
 *
 * %sccs.include.redist.c%
 */

#ifndef lint
static char sccsid[] = "@(#)clrtoeol.c	8.1 (Berkeley) %G%";
#endif	/* not lint */

#include <curses.h>

/*
 * wclrtoeol --
 *	Clear up to the end of line.
 */
int
wclrtoeol(win)
	register WINDOW *win;
{
	register int minx, x, y;
	register __LDATA *end, *maxx, *sp;

	y = win->cury;
	x = win->curx;
	if (win->lines[y]->flags & __ISPASTEOL) {
		if (y < win->maxy - 1) {
			y++;
			x = 0;
		} else
			return (OK);
	}
	end = &win->lines[y]->line[win->maxx];
	minx = -1;
	maxx = &win->lines[y]->line[x];
	for (sp = maxx; sp < end; sp++)
		if (sp->ch != ' ' || sp->attr != 0) {
			maxx = sp;
			if (minx == -1)
				minx = sp - win->lines[y]->line;
			sp->ch = ' ';
			sp->attr = 0;
		}
#ifdef DEBUG
	__CTRACE("CLRTOEOL: minx = %d, maxx = %d, firstch = %d, lastch = %d\n",
	    minx, maxx - win->lines[y]->line, *win->lines[y]->firstchp, 
	    *win->lines[y]->lastchp);
#endif
	/* Update firstch and lastch for the line. */
	return (__touchline(win, y, x, win->maxx - 1, 0));
}






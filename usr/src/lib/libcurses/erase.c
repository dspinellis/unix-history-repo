/*
 * Copyright (c) 1981, 1993
 *	The Regents of the University of California.  All rights reserved.
 *
 * %sccs.include.redist.c%
 */

#ifndef lint
static char sccsid[] = "@(#)erase.c	8.1 (Berkeley) %G%";
#endif	/* not lint */

#include <curses.h>

/*
 * werase --
 *	Erases everything on the window.
 */
int
werase(win)
	register WINDOW *win;
{

	register int minx, y;
	register __LDATA *sp, *end, *start, *maxx;

#ifdef DEBUG
	__CTRACE("werase: (%0.2o)\n", win);
#endif
	for (y = 0; y < win->maxy; y++) {
		minx = -1;
		start = win->lines[y]->line;
		end = &start[win->maxx];
		for (sp = start; sp < end; sp++)
			if (sp->ch != ' ' || sp->attr != 0) {
				maxx = sp; 
				if (minx == -1)
					minx = sp - start;
				sp->ch = ' ';
				sp->attr = 0;
			}
		if (minx != -1)
			__touchline(win, y, minx, maxx - win->lines[y]->line,
			   0);
	}
	return (OK);
}

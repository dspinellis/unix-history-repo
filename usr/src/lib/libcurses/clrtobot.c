/*
 * Copyright (c) 1981 Regents of the University of California.
 * All rights reserved.
 *
 * %sccs.include.redist.c%
 */

#ifndef lint
static char sccsid[] = "@(#)clrtobot.c	5.10 (Berkeley) %G%";
#endif	/* not lint */

#include <curses.h>

/*
 * wclrtobot --
 *	Erase everything on the window.
 */
int
wclrtobot(win)
	register WINDOW *win;
{
	register int minx, startx, y;
	register __LDATA *sp, *end, *maxx;

	startx = win->curx;
	for (y = win->cury; y < win->maxy; y++) {
		minx = -1;
		end = &win->lines[y]->line[win->maxx];
		for (sp = &win->lines[y]->line[startx]; sp < end; sp++)
			if (sp->ch != ' ' || sp->attr != 0) {
				maxx = sp;
				if (minx == -1)
					minx = sp - win->lines[y]->line;
				sp->ch = ' ';
				sp->attr = 0;
			}
		if (minx != -1)
			__touchline(win, y, minx, maxx - win->lines[y]->line,
		            0);
		startx = 0;
	}
	return (OK);
}

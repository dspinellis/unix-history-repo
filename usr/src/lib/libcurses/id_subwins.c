/*
 * Copyright (c) 1981 Regents of the University of California.
 * All rights reserved.
 *
 * %sccs.include.redist.c%
 */

#ifndef lint
static char sccsid[] = "@(#)id_subwins.c	5.7 (Berkeley) %G%";
#endif	/* not lint */

#include <curses.h>

/*
 * __id_subwins --
 *	Re-sync the pointers to lines for all the subwindows.
 */
void
__id_subwins(orig)
	register WINDOW *orig;
{
	register WINDOW *win;
	register int oy, realy, y;

	realy = orig->begy + orig->cury;
	for (win = orig->nextp; win != orig; win = win->nextp) {
		/*
		 * If the window ends before our current position, don't need
		 * to do anything.
		 */
		if (win->begy + win->maxy <= realy)
			continue;

		oy = orig->cury;
		for (y = realy - win->begy; y < win->maxy; y++, oy++)
			win->lines[y]->line = 
				&orig->lines[oy]->line[win->ch_off];
	}
}

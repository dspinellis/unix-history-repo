/*
 * Copyright (c) 1981 Regents of the University of California.
 * All rights reserved.
 *
 * %sccs.include.redist.c%
 */

#ifndef lint
static char sccsid[] = "@(#)id_subwins.c	5.5 (Berkeley) %G%";
#endif	/* not lint */

#include <curses.h>

/*
 * __id_subwins --
 *	Re-sync the pointers to _y for all the subwindows.
 */
void
__id_subwins(orig)
	register WINDOW *orig;
{
	register WINDOW *win;
	register int oy, realy, x, y;

	realy = orig->_begy + orig->_cury;
	for (win = orig->_nextp; win != orig; win = win->_nextp) {
		/*
		 * If the window ends before our current position, don't need
		 * to do anything.
		 */
		if (win->_begy + win->_maxy <= realy)
			continue;

		oy = orig->_cury;
		for (y = realy - win->_begy; y < win->_maxy; y++, oy++)
			win->_y[y] = &orig->_y[oy][win->_ch_off];
	}
}

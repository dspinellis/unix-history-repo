/*
 * Copyright (c) 1981 Regents of the University of California.
 * All rights reserved.
 *
 * %sccs.include.redist.c%
 */

#ifndef lint
static char sccsid[] = "@(#)delwin.c	5.5 (Berkeley) %G%";
#endif	/* not lint */

#include <curses.h>

/*
 * delwin --
 *	Delete a window and release it back to the system.
 */
int
delwin(win)
	register WINDOW *win;
{

	register WINDOW *wp, *np;
	register int i;

	if (win->_orig == NULL) {
		/*
		 * If we are the original window, delete the space for all
		 * the subwindows, and the array of space as well.
		 */
		for (i = 0; i < win->_maxy && win->_y[i]; i++)
			free(win->_y[i]);
		free(win->_firstch);
		free(win->_lastch);
		wp = win->_nextp;
		while (wp != win) {
			np = wp->_nextp;
			delwin(wp);
			wp = np;
		}
	} else {
		/*
		 * If we are a subwindow, take ourselves out of the list.
		 * NOTE: if we are a subwindow, the minimum list is orig
		 * followed by this subwindow, so there are always at least
		 * two windows in the list.
		 */
		for (wp = win->_nextp; wp->_nextp != win; wp = wp->_nextp)
			continue;
		wp->_nextp = win->_nextp;
	}
	free(win->_y);
	free(win);
	return (OK);
}

/*
 * Copyright (c) 1980 Regents of the University of California.
 * All rights reserved.  The Berkeley software License Agreement
 * specifies the terms and conditions for redistribution.
 */

#ifndef lint
static char sccsid[] = "@(#)delwin.c	5.1 (Berkeley) %G%";
#endif not lint

# include	"curses.ext"

/*
 *	This routine deletes a window and releases it back to the system.
 *
 */
delwin(win)
reg WINDOW	*win; {

	reg int		i;
	reg WINDOW	*wp, *np;

	if (win->_orig == NULL) {
		/*
		 * If we are the original window, delete the space for
		 * all the subwindows, and the array of space as well.
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
	}
	else {
		/*
		 * If we are a subwindow, take ourselves out of the
		 * list.  NOTE: if we are a subwindow, the minimum list
		 * is orig followed by this subwindow, so there are
		 * always at least two windows in the list.
		 */
		for (wp = win->_nextp; wp->_nextp != win; wp = wp->_nextp)
			continue;
		wp->_nextp = win->_nextp;
	}
	free(win->_y);
	free(win);
}

/*
 * Copyright (c) 1981 Regents of the University of California.
 * All rights reserved.
 *
 * %sccs.include.redist.c%
 */

#ifndef lint
static char sccsid[] = "@(#)delwin.c	5.7 (Berkeley) %G%";
#endif	/* not lint */

#include <curses.h>
#include <stdlib.h>

/*
 * delwin --
 *	Delete a window and release it back to the system.
 */
int
delwin(win)
	register WINDOW *win;
{

	register WINDOW *wp, *np;
	register LINE *lp;
	register int i;

	if (win->orig == NULL) {
		/*
		 * If we are the original window, delete the space for all
		 * the subwindows, and the array of space as well which is
		 * pointed to by win->topline->line.
		 */

		for (lp = win->topline, i = 0; i < win->maxy; i++) 
			free(lp);
		free(win->wspace);
		free(win->lines);
		wp = win->nextp;
		while (wp != win) {
			np = wp->nextp;
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
		for (wp = win->nextp; wp->nextp != win; wp = wp->nextp)
			continue;
		wp->nextp = win->nextp;
	}
	free(win);
	return (OK);
}

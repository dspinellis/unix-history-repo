/*
 * Copyright (c) 1981 Regents of the University of California.
 * All rights reserved.
 *
 * %sccs.include.redist.c%
 */

#ifndef lint
static char sccsid[] = "@(#)insertln.c	5.5 (Berkeley) %G%";
#endif	/* not lint */

#include <curses.h>

/*
 * winsertln --
 *	Do an insert-line on the window, leaving (_cury,_curx) unchanged.
 */
int
winsertln(win)
	register WINDOW *win;
{

	register int x, y;
	register char *end, *temp;

#ifdef DEBUG
	__TRACE("insertln: (%0.2o)\n", win);
#endif
	if (win->_orig == NULL)
		temp = win->_y[win->_maxy - 1];
	for (y = win->_maxy - 1; y > win->_cury; --y) {
		if (win->_orig == NULL)
			win->_y[y] = win->_y[y - 1];
		else
			bcopy(win->_y[y - 1], win->_y[y], win->_maxx);
		touchline(win, y, 0, win->_maxx - 1);
	}
	if (win->_orig == NULL)
		win->_y[y] = temp;
	else
		temp = win->_y[y];
	for (end = &temp[win->_maxx]; temp < end;)
		*temp++ = ' ';
	touchline(win, y, 0, win->_maxx - 1);
	if (win->_orig == NULL)
		__id_subwins(win);
	return (OK);
}

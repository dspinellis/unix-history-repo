/*
 * Copyright (c) 1981 Regents of the University of California.
 * All rights reserved.
 *
 * %sccs.include.redist.c%
 */

#ifndef lint
static char sccsid[] = "@(#)deleteln.c	5.6 (Berkeley) %G%";
#endif	/* not lint */

#include <curses.h>

/*
 * wdeleteln --
 *	Delete a line from the screen.  It leaves (_cury, _curx) unchanged.
 */
int
wdeleteln(win)
	register WINDOW *win;
{
	register int x, y;
	register char *temp;

#ifdef DEBUG
	__TRACE("deleteln: (%0.2o)\n", win);
#endif
	temp = win->_y[win->_cury];
	for (y = win->_cury; y < win->_maxy - 1; y++) {
		if (win->_orig == NULL)
			win->_y[y] = win->_y[y + 1];
		else
			bcopy(win->_y[y + 1], win->_y[y], win->_maxx);
		touchline(win, y, 0, win->_maxx - 1);
	}
	if (win->_orig == NULL)
		win->_y[y] = temp;
	else
		temp = win->_y[y];
	(void)memset(temp, ' ', &temp[win->_maxx] - temp);
	touchline(win, win->_cury, 0, win->_maxx - 1);
	if (win->_orig == NULL)
		__id_subwins(win);
	return (OK);
}

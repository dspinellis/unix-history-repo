/*
 * Copyright (c) 1981 Regents of the University of California.
 * All rights reserved.
 *
 * %sccs.include.redist.c%
 */

#ifndef lint
static char sccsid[] = "@(#)insch.c	5.5 (Berkeley) %G%";
#endif	/* not lint */

#include <curses.h>

/*
 * winsch --
 *	Do an insert-char on the line, leaving (_cury,_curx) unchanged.
 */
int
winsch(win, ch)
	register WINDOW *win;
	int ch;
{

	register char *end, *temp1, *temp2;

	end = &win->_y[win->_cury][win->_curx];
	temp1 = &win->_y[win->_cury][win->_maxx - 1];
	temp2 = temp1 - 1;
	while (temp1 > end)
		*temp1-- = *temp2--;
	*temp1 = ch;
	touchline(win, win->_cury, win->_curx, win->_maxx - 1);
	if (win->_cury == LINES - 1 && win->_y[LINES - 1][COLS - 1] != ' ')
		if (win->_scroll) {
			wrefresh(win);
			scroll(win);
			win->_cury--;
		} else
			return (ERR);
	return (OK);
}

/*
 * Copyright (c) 1980 Regents of the University of California.
 * All rights reserved.  The Berkeley software License Agreement
 * specifies the terms and conditions for redistribution.
 */

#ifndef lint
static char sccsid[] = "@(#)delch.c	5.1 (Berkeley) %G%";
#endif not lint

# include	"curses.ext"

/*
 *	This routine performs an insert-char on the line, leaving
 * (_cury,_curx) unchanged.
 *
 */
wdelch(win)
reg WINDOW	*win; {

	reg char	*temp1, *temp2;
	reg char	*end;
	reg int		lch;

	end = &win->_y[win->_cury][win->_maxx - 1];
	temp1 = &win->_y[win->_cury][win->_curx];
	temp2 = temp1 + 1;
	while (temp1 < end)
		*temp1++ = *temp2++;
	*temp1 = ' ';
	touchline(win, win->_cury, win->_curx, win->_maxx - 1);
	return OK;
}

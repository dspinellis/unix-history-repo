/*
 * Copyright (c) 1980 Regents of the University of California.
 * All rights reserved.  The Berkeley software License Agreement
 * specifies the terms and conditions for redistribution.
 */

#ifndef lint
static char sccsid[] = "@(#)clear.c	5.1 (Berkeley) 6/7/85";
#endif not lint

# include	"curses.ext"

/*
 *	This routine clears the window.
 *
 */
wclear(win)
reg WINDOW	*win; {

	werase(win);
	win->_clear = TRUE;
	return OK;
}

/*
 * Copyright (c) 1981 Regents of the University of California.
 * All rights reserved.
 *
 * %sccs.include.redist.c%
 */

#ifndef lint
static char sccsid[] = "@(#)clear.c	5.7 (Berkeley) %G%";
#endif	/* not lint */

#include <curses.h>

/*
 * wclear --
 *	Clear the window.
 */
int
wclear(win)
	register WINDOW *win;
{
	if (werase(win) == CURSES_OK) {
		win->flags |= __CLEAROK;
		return (CURSES_OK);
	}
	return (CURSES_ERR);
}

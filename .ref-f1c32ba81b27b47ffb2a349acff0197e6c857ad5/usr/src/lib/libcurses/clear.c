/*
 * Copyright (c) 1981 Regents of the University of California.
 * All rights reserved.
 *
 * %sccs.include.redist.c%
 */

#ifndef lint
static char sccsid[] = "@(#)clear.c	5.4 (Berkeley) %G%";
#endif /* not lint */

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

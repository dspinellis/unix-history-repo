/*
 * Copyright (c) 1981 Regents of the University of California.
 * All rights reserved.
 *
 * Redistribution and use in source and binary forms are permitted
 * provided that this notice is preserved and that due credit is given
 * to the University of California at Berkeley. The name of the University
 * may not be used to endorse or promote products derived from this
 * software without specific prior written permission. This software
 * is provided ``as is'' without express or implied warranty.
 */

#ifndef lint
static char sccsid[] = "@(#)move.c	5.3 (Berkeley) %G%";
#endif /* not lint */

# include	"curses.ext"

/*
 *	This routine moves the cursor to the given point
 *
 */
wmove(win, y, x)
reg WINDOW	*win;
reg int		y, x; {

# ifdef DEBUG
	fprintf(outf, "MOVE to (%d, %d)\n", y, x);
# endif
	if (x < 0 || y < 0)
		return ERR;
	if (x >= win->_maxx || y >= win->_maxy)
		return ERR;
	win->_curx = x;
	win->_cury = y;
	return OK;
}

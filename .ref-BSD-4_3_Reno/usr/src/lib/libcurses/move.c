/*
 * Copyright (c) 1981 Regents of the University of California.
 * All rights reserved.
 *
 * Redistribution and use in source and binary forms are permitted
 * provided that: (1) source distributions retain this entire copyright
 * notice and comment, and (2) distributions including binaries display
 * the following acknowledgement:  ``This product includes software
 * developed by the University of California, Berkeley and its contributors''
 * in the documentation or other materials provided with the distribution
 * and in all advertising materials mentioning features or use of this
 * software. Neither the name of the University nor the names of its
 * contributors may be used to endorse or promote products derived
 * from this software without specific prior written permission.
 * THIS SOFTWARE IS PROVIDED ``AS IS'' AND WITHOUT ANY EXPRESS OR
 * IMPLIED WARRANTIES, INCLUDING, WITHOUT LIMITATION, THE IMPLIED
 * WARRANTIES OF MERCHANTABILITY AND FITNESS FOR A PARTICULAR PURPOSE.
 */

#ifndef lint
static char sccsid[] = "@(#)move.c	5.5 (Berkeley) 6/1/90";
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

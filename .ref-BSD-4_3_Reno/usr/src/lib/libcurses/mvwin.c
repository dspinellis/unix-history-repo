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
static char sccsid[] = "@(#)mvwin.c	5.4 (Berkeley) 6/1/90";
#endif /* not lint */

# include	"curses.ext"

/*
 * relocate the starting position of a window
 *
 */

mvwin(win, by, bx)
reg WINDOW	*win;
reg int		by, bx; {

	register WINDOW	*orig;
	register int	dy, dx;

	if (by + win->_maxy > LINES || bx + win->_maxx > COLS)
		return ERR;
	dy = by - win->_begy;
	dx = bx - win->_begx;
	orig = win->_orig;
	if (orig == NULL) {
		orig = win;
		do {
			win->_begy += dy;
			win->_begx += dx;
			_swflags_(win);
			win = win->_nextp;
		} while (win != orig);
	}
	else {
		if (by < orig->_begy || win->_maxy + dy > orig->_maxy)
			return ERR;
		if (bx < orig->_begx || win->_maxx + dx > orig->_maxx)
			return ERR;
		win->_begy = by;
		win->_begx = bx;
		_swflags_(win);
		_set_subwin_(orig, win);
	}
	touchwin(win);
	return OK;
}

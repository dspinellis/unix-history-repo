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
static char sccsid[] = "@(#)mvwin.c	5.2 (Berkeley) %G%";
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

/*
 * Copyright (c) 1980 Regents of the University of California.
 * All rights reserved.  The Berkeley software License Agreement
 * specifies the terms and conditions for redistribution.
 */

#ifndef lint
static char sccsid[] = "@(#)endwin.c	5.1 (Berkeley) %G%";
#endif not lint

/*
 * Clean things up before exiting
 *
 */

# include	"curses.ext"

endwin()
{
	resetty();
	_puts(VE);
	_puts(TE);
	if (curscr) {
		if (curscr->_flags & _STANDOUT) {
			_puts(SE);
			curscr->_flags &= ~_STANDOUT;
		}
		_endwin = TRUE;
	}
}

/*
 * Copyright (c) 1987 Regents of the University of California.
 * All rights reserved.
 *
 * %sccs.include.redist.c%
 */

#ifndef lint
static char sccsid[] = "@(#)addbytes.c	5.6 (Berkeley) %G%";
#endif	/* not lint */

#include <curses.h>

#define	SYNCH_IN	{y = win->_cury; x = win->_curx;}
#define	SYNCH_OUT	{win->_cury = y; win->_curx = x;}

/*
 * waddbytes --
 *	Add the character to the current position in the given window.
 */
waddbytes(win, bytes, count)
	register WINDOW *win;
	register char *bytes;
	register int count;
{
	static char blanks[] = "        ";
	register int c, newx, x, y;

	SYNCH_IN;
#ifdef DEBUG
	__TRACE("ADDBYTES('%c') at (%d, %d)\n", c, y, x);
#endif
	while (count--) {
		c = *bytes++;
		switch (c) {
		case '\t':
			SYNCH_OUT;
			if (waddbytes(win, blanks, 8 - (x % 8)) == ERR)
				return (ERR);
			SYNCH_IN;
			break;

		default:
#ifdef DEBUG
	__TRACE("ADDBYTES: 1: y = %d, x = %d, firstch = %d, lastch = %d\n",
	    y, x, win->_firstch[y], win->_lastch[y]);
#endif
			if (win->_flags & _STANDOUT)
				c |= _STANDOUT;
#ifdef DEBUG
	__TRACE("ADDBYTES(%0.2o, %d, %d)\n", win, y, x);
#endif
			if (win->_y[y][x] != c) {
				newx = x + win->_ch_off;
				if (win->_firstch[y] == _NOCHANGE)
					win->_firstch[y] =
					    win->_lastch[y] = newx;
				else if (newx < win->_firstch[y])
					win->_firstch[y] = newx;
				else if (newx > win->_lastch[y])
					win->_lastch[y] = newx;
#ifdef DEBUG
	__TRACE("ADDBYTES: change gives f/l: %d/%d [%d/%d]\n",
	    win->_firstch[y], win->_lastch[y],
	    win->_firstch[y] - win->_ch_off,
	    win->_lastch[y] - win->_ch_off);
#endif
			}
			win->_y[y][x++] = c;
			if (x > win->_maxx) {
				x = 0;
newline:			if (++y > win->_maxy)
					if (win->_scroll) {
						SYNCH_OUT;
						scroll(win);
						SYNCH_IN;
						--y;
					} else
						return (ERR);
			}
#ifdef DEBUG
	__TRACE("ADDBYTES: 2: y = %d, x = %d, firstch = %d, lastch = %d\n",
	    y, x, win->_firstch[y], win->_lastch[y]);
#endif
			break;
		case '\n':
			SYNCH_OUT;
			wclrtoeol(win);
			SYNCH_IN;
			if (!NONL)
				x = 0;
			goto newline;
		case '\r':
			x = 0;
			break;
		case '\b':
			if (--x < 0)
				x = 0;
			break;
		}
	}
	SYNCH_OUT;
	return (OK);
}

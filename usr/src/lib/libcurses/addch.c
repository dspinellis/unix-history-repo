/*
 * Copyright (c) 1980 Regents of the University of California.
 * All rights reserved.  The Berkeley software License Agreement
 * specifies the terms and conditions for redistribution.
 */

#ifndef lint
static char sccsid[] = "@(#)addch.c	5.1 (Berkeley) %G%";
#endif not lint

# include	"curses.ext"

/*
 *	This routine adds the character to the current position
 *
 */
waddch(win, c)
reg WINDOW	*win;
char		c;
{
	reg int		x, y;
	reg WINDOW	*wp;
	reg int		newx;

	x = win->_curx;
	y = win->_cury;
# ifdef FULLDEBUG
	fprintf(outf, "ADDCH('%c') at (%d, %d)\n", c, y, x);
# endif
	switch (c) {
	  case '\t':
		for (newx = x + (8 - (x & 07)); x < newx; x++)
			if (waddch(win, ' ') == ERR)
				return ERR;
		return OK;

	  default:
# ifdef FULLDEBUG
		fprintf(outf, "ADDCH: 1: y = %d, x = %d, firstch = %d, lastch = %d\n", y, x, win->_firstch[y], win->_lastch[y]);
# endif
		if (win->_flags & _STANDOUT)
			c |= _STANDOUT;
		set_ch(win, y, x, c);
		win->_y[y][x++] = c;
		if (x >= win->_maxx) {
			x = 0;
newline:
			if (++y >= win->_maxy)
				if (win->_scroll) {
					scroll(win);
					--y;
				}
				else
					return ERR;
		}
# ifdef FULLDEBUG
		fprintf(outf, "ADDCH: 2: y = %d, x = %d, firstch = %d, lastch = %d\n", y, x, win->_firstch[y], win->_lastch[y]);
# endif
		break;
	  case '\n':
		wclrtoeol(win);
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
	win->_curx = x;
	win->_cury = y;
	return OK;
}

/*
 * set_ch:
 *	Set the first and last change flags for this window.
 */
static
set_ch(win, y, x, ch)
reg WINDOW	*win;
int		y, x;
{
# ifdef	FULLDEBUG
	fprintf(outf, "SET_CH(%0.2o, %d, %d)\n", win, y, x);
# endif
	if (win->_y[y][x] != ch) {
		x += win->_ch_off;
		if (win->_firstch[y] == _NOCHANGE)
			win->_firstch[y] = win->_lastch[y] = x;
		else if (x < win->_firstch[y])
			win->_firstch[y] = x;
		else if (x > win->_lastch[y])
			win->_lastch[y] = x;
# ifdef FULLDEBUG
		fprintf(outf, "SET_CH: change gives f/l: %d/%d [%d/%d]\n",
			win->_firstch[y], win->_lastch[y],
			win->_firstch[y] - win->_ch_off,
			win->_lastch[y] - win->_ch_off);
# endif
	}
}

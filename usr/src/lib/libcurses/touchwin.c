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
static char sccsid[] = "@(#)touchwin.c	5.2 (Berkeley) %G%";
#endif /* not lint */

# include	"curses.ext"

/*
 * make it look like the whole window has been changed.
 *
 */
touchwin(win)
register WINDOW	*win;
{
	register int	y, maxy;

# ifdef	DEBUG
	fprintf(outf, "TOUCHWIN(%0.2o)\n", win);
# endif
	maxy = win->_maxy;
	for (y = 0; y < maxy; y++)
		touchline(win, y, 0, win->_maxx - 1);
}

/*
 * touch a given line
 */
touchline(win, y, sx, ex)
register WINDOW	*win;
register int	y, sx, ex;
{
# ifdef DEBUG
	fprintf(outf, "TOUCHLINE(%0.2o, %d, %d, %d)\n", win, y, sx, ex);
	fprintf(outf, "TOUCHLINE:first = %d, last = %d\n", win->_firstch[y], win->_lastch[y]);
# endif
	sx += win->_ch_off;
	ex += win->_ch_off;
	if (win->_firstch[y] == _NOCHANGE) {
		win->_firstch[y] = sx;
		win->_lastch[y] = ex;
	}
	else {
		if (win->_firstch[y] > sx)
			win->_firstch[y] = sx;
		if (win->_lastch[y] < ex)
			win->_lastch[y] = ex;
	}
# ifdef	DEBUG
	fprintf(outf, "TOUCHLINE:first = %d, last = %d\n", win->_firstch[y], win->_lastch[y]);
# endif
}

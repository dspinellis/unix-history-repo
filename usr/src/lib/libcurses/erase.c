/*
 * Copyright (c) 1980 Regents of the University of California.
 * All rights reserved.  The Berkeley software License Agreement
 * specifies the terms and conditions for redistribution.
 */

#ifndef lint
static char sccsid[] = "@(#)erase.c	5.1 (Berkeley) %G%";
#endif not lint

# include	"curses.ext"

/*
 *	This routine erases everything on the window.
 *
 */
werase(win)
reg WINDOW	*win; {

	reg int		y;
	reg char	*sp, *end, *start, *maxx;
	reg int		minx;

# ifdef DEBUG
	fprintf(outf, "WERASE(%0.2o)\n", win);
# endif
	for (y = 0; y < win->_maxy; y++) {
		minx = _NOCHANGE;
		start = win->_y[y];
		end = &start[win->_maxx];
		for (sp = start; sp < end; sp++)
			if (*sp != ' ') {
				maxx = sp;
				if (minx == _NOCHANGE)
					minx = sp - start;
				*sp = ' ';
			}
		if (minx != _NOCHANGE)
			touchline(win, y, minx, maxx - win->_y[y]);
	}
	win->_curx = win->_cury = 0;
}

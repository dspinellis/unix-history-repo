/*
 * Copyright (c) 1980 Regents of the University of California.
 * All rights reserved.  The Berkeley software License Agreement
 * specifies the terms and conditions for redistribution.
 */

#ifndef lint
static char sccsid[] = "@(#)scroll.c	5.1 (Berkeley) %G%";
#endif not lint

# include	"curses.ext"

/*
 *	This routine scrolls the window up a line.
 *
 */
scroll(win)
register  WINDOW	*win;
{
	register int	oy, ox;

# ifdef DEBUG
	fprintf(outf, "SCROLL(%0.2o)\n", win);
# endif

	if (!win->_scroll)
		return ERR;

	getyx(win, oy, ox);
	wmove(win, 0, 0);
	wdeleteln(win);
	wmove(win, oy, ox);

	if (win == curscr) {
		_putchar('\n');
		if (!NONL)
			win->_curx = 0;
# ifdef DEBUG
		fprintf(outf, "SCROLL: win == curscr\n");
# endif
	}
}

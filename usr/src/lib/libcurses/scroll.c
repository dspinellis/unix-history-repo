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
static char sccsid[] = "@(#)scroll.c	5.2 (Berkeley) %G%";
#endif /* not lint */

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

# include	"curses.ext"

/*
 *	This routine deletes a window and releases it back to the system.
 *
 * %G% (Berkeley) @(#)delwin.c	1.3
 */
delwin(win)
reg WINDOW	*win; {

	reg int		i;
	reg WINDOW	*wp, *np;

	if (win->_orig == NULL) {
		for (i = 0; i < win->_maxy && win->_y[i]; i++)
			cfree(win->_y[i]);
		wp = win->_nextp;
		while (wp != win) {
			np = wp->_nextp;
			delwin(wp);
			wp = np;
		}
	}
	cfree(win->_y);
	cfree(win->_firstch);
	cfree(win->_lastch);
	cfree(win);
}

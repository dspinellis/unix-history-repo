# include	"curses.ext"

/*
 * make it look like the whole window has been changed.
 *
 * %G% (Berkeley) @(#)touchwin.c	1.2
 */
touchwin(win)
reg WINDOW	*win;
{
	reg WINDOW	*wp;

	do_touch(win);
	for (wp = win->_nextp; wp != win; wp = wp->_nextp)
		do_touch(wp);
}

/*
 * do_touch:
 *	Touch the window
 */
static
do_touch(win)
reg WINDOW	*win; {

	reg int		y, maxy, maxx;

	maxy = win->_maxy;
	maxx = win->_maxx - 1;
	for (y = 0; y < maxy; y++) {
		win->_firstch[y] = 0;
		win->_lastch[y] = maxx;
	}
}

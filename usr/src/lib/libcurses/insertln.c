# include	"curses.ext"

/*
 *	This routine performs an insert-line on the window, leaving
 * (_cury,_curx) unchanged.
 *
 * %G% (Berkeley) @(#)insertln.c	1.3
 */
winsertln(win)
reg WINDOW	*win; {

	reg char	*temp;
	reg int		y;
	reg char	*end;

	temp = win->_y[win->_maxy-1];
	win->_firstch[win->_cury] = 0;
	win->_lastch[win->_cury] = win->_maxx - 1;
	for (y = win->_maxy - 1; y > win->_cury; --y) {
		win->_y[y] = win->_y[y-1];
		win->_firstch[y] = 0;
		win->_lastch[y] = win->_maxx - 1;
	}
	for (end = &temp[win->_maxx]; temp < end; )
		*temp++ = ' ';
	win->_y[win->_cury] = temp - win->_maxx;
}

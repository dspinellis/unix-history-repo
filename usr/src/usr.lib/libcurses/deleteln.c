# include	"curses.ext"

/*
 *	This routine deletes a line from the screen.  It leaves
 * (_cury,_curx) unchanged.
 *
 * 5/11/81 (Berkeley) @(#)deleteln.c	1.4
 */
wdeleteln(win)
reg WINDOW	*win; {

	reg char	*temp;
	reg int		y;
	reg char	*end;

	temp = win->_y[win->_cury];
	for (y = win->_cury; y < win->_maxy - 1; y++) {
		win->_y[y] = win->_y[y+1];
		win->_firstch[y] = 0;
		win->_lastch[y] = win->_maxx - 1;
	}
	for (end = &temp[win->_maxx]; temp < end; )
		*temp++ = ' ';
	win->_y[win->_maxy-1] = temp - win->_maxx;
}

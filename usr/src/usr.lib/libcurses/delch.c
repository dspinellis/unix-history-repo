# include	"curses.ext"

/*
 *	This routine performs an insert-char on the line, leaving
 * (_cury,_curx) unchanged.
 *
 * @(#)delch.c	1.2 (Berkeley) 5/11/81
 */
wdelch(win)
reg WINDOW	*win; {

	reg char	*temp1, *temp2;
	reg char	*end;

	end = &win->_y[win->_cury][win->_maxx - 1];
	temp1 = &win->_y[win->_cury][win->_curx];
	temp2 = temp1 + 1;
	while (temp1 < end)
		*temp1++ = *temp2++;
	*temp1 = ' ';
	win->_lastch[win->_cury] = win->_maxx - 1;
	if (win->_firstch[win->_cury] == _NOCHANGE ||
	    win->_firstch[win->_cury] > win->_curx)
		win->_firstch[win->_cury] = win->_curx;
	return OK;
}

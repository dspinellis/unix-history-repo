# include	"curses.ext"

/*
 *	This routine moves the cursor to the given point
 *
 * 1/26/81 (Berkeley) @(#)move.c	1.1
 */
wmove(win, y, x)
reg WINDOW	*win;
reg int		y, x; {

# ifdef DEBUG
	fprintf(outf, "MOVE to (%d, %d)\n", y, x);
# endif
	if (x >= win->_maxx || y >= win->_maxy)
		return ERR;
	win->_curx = x;
	win->_cury = y;
	return OK;
}

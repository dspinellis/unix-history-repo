# include	"curses.ext"

/*
 *	This routine clears the window.
 *
 * 1/26/81 (Berkeley) @(#)clear.c	1.1
 */
wclear(win)
reg WINDOW	*win; {

	if (win == curscr) {
# ifdef DEBUG
		fprintf(outf,"WCLEAR: win == curscr\n");
		fprintf(outf,"WCLEAR: curscr = %d\n",curscr);
		fprintf(outf,"WCLEAR: stdscr = %d\n",stdscr);
# endif
		clear();
		return refresh();
	}
	werase(win);
	win->_clear = TRUE;
	return OK;
}

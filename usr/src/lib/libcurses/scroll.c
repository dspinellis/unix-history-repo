# include	"curses.ext"

/*
 *	This routine scrolls the window up a line.
 *
 * @(#)scroll.c	1.6 (Berkeley) %G%
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

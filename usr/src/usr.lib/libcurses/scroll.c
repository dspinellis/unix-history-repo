# include	"curses.ext"

/*
 *	This routine scrolls the window up a line.
 *
 * 5/11/81 (Berkeley) @(#)scroll.c	1.2
 */
scroll(win)
reg WINDOW	*win; {

	reg char	*sp;
	reg int		i;
	reg char	*temp;

	if (!win->_scroll)
		return ERR;
	temp = win->_y[0];
	for (i = 0; i < win->_maxy - 2; i++)
		win->_y[i] = win->_y[i+1];
	for (sp = temp; sp - temp < win->_maxx; )
		*sp++ = ' ';
	win->_y[win->_maxy - 1] = temp;
	win->_cury--;
	if (win == curscr) {
		putchar('\n');
		if (!NONL)
			win->_curx = 0;
# ifdef DEBUG
		fprintf(outf, "SCROLL: win == curscr\n");
# endif
	}
# ifdef DEBUG
	else
		fprintf(outf, "SCROLL: win [0%o] != curscr [0%o]\n",win,curscr);
# endif
	return OK;
}

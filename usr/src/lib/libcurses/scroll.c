# include	"curses.ext"

/*
 *	This routine scrolls the window up a line.
 *
 * @(#)scroll.c	1.5 (Berkeley) %G%
 */
scroll(win)
reg WINDOW	*win; {

	reg char	*sp;
	reg int		i;
	reg char	*temp;

	if (!win->_scroll)
		return ERR;
	temp = win->_y[0];
	for (i = 1; i < win->_maxy; i++)
		win->_y[i - 1] = win->_y[i];
	for (sp = temp; sp < &temp[win->_maxx]; )
		*sp++ = ' ';
	win->_y[win->_maxy - 1] = temp;
	if (win == curscr) {
		_putchar('\n');
		if (!NONL)
			win->_curx = 0;
# ifdef DEBUG
		fprintf(outf, "SCROLL: win == curscr\n");
		fflush(outf);
# endif
	} else {
		if(win->_cury-- <= 0)
			win->_cury = 0;
# ifdef DEBUG
		fprintf(outf, "SCROLL: win [0%o] != curscr [0%o]\n",win,curscr);
		fflush(outf);
# endif
	}
	touchwin(win);
	return OK;
}

/*
 * allocate space for and set up defaults for a new window
 *
 * 10/26/81 (Berkeley) %W
 */

# include	"curses.ext"

short	*calloc();
WINDOW	*malloc();

static WINDOW	*makenew();

# undef		nl	/* don't need it here, and it interferes	*/

WINDOW *
newwin(num_lines, num_cols, begy, begx)
int	num_lines, num_cols, begy, begx;
{
	reg WINDOW	*win;
	reg char	*sp;
	reg int		i, by, bx, nl, nc;

	by = begy;
	bx = begx;
	nl = num_lines;
	nc = num_cols;

	if (nl == 0)
		nl = LINES - by;
	if (nc == 0)
		nc = COLS - bx;
	if ((win = makenew(nl, nc, by, bx)) == NULL)
		return ERR;
	for (i = 0; i < nl; i++)
		if ((win->_y[i] = (char *) calloc(nc, sizeof (char))) == NULL) {
			reg int		j;

			for (j = 0; j < i; j++)
				cfree(win->_y[j]);
			cfree(win->_firstch);
			cfree(win->_lastch);
			cfree(win->_y);
			cfree(win);
			return ERR;
		}
		else
			for (sp = win->_y[i]; sp < win->_y[i] + nc; )
				*sp++ = ' ';
	return win;
}

WINDOW *
subwin(orig, num_lines, num_cols, begy, begx)
reg WINDOW	*orig;
int		num_lines, num_cols, begy, begx; {

	reg int		i;
	reg WINDOW	*win;
	reg int		by, bx, nl, nc;
	reg int		j, k;

	by = begy;
	bx = begx;
	nl = num_lines;
	nc = num_cols;

	/*
	 * make sure window fits inside the original one
	 */
# ifdef	DEBUG
	fprintf(outf, "SUBWIN(%0.2o, %d, %d, %d, %d)\n", orig, nl, nc, by, bx);
# endif
	if (by < orig->_begy || bx < orig->_begx
	    || by + nl > orig->_maxy || bx + nc > orig->_maxx)
		return ERR;
	if (nl == 0)
		nl = orig->_maxy - orig->_begy - by;
	if (nc == 0)
		nc = orig->_maxx - orig->_begx - bx;
	if ((win = makenew(nl, nc, by, bx)) == NULL)
		return ERR;
	j = orig->_begy + by;
	k = orig->_begx + bx;
	for (i = 0; i < nl; i++)
		win->_y[i] = &orig->_y[j++][k];
	win->_flags = _SUBWIN;
	return win;
}

/*
 *	This routine sets up a window buffer and returns a pointer to it.
 */
static WINDOW *
makenew(num_lines, num_cols, begy, begx)
int	num_lines, num_cols, begy, begx; {

	reg int		i;
	reg WINDOW	*win;
	reg int		by, bx, nl, nc;

	by = begy;
	bx = begx;
	nl = num_lines;
	nc = num_cols;

# ifdef	DEBUG
	fprintf(outf, "MAKENEW(%d, %d, %d, %d)\n", nl, nc, by, bx);
# endif
	if ((win = (WINDOW *) calloc(1, sizeof (WINDOW))) == NULL)
		return NULL;
# ifdef DEBUG
	fprintf(outf, "MAKENEW: nl = %d\n", nl);
# endif
	if ((win->_y = (char **) calloc(nl, sizeof (char *))) == NULL) {
		cfree(win);
		return NULL;
	}
	if ((win->_firstch = calloc(nl, sizeof (short))) == NULL) {
		cfree(win);
		cfree(win->_y);
		return NULL;
	}
	if ((win->_lastch = calloc(nl, sizeof (short))) == NULL) {
		cfree(win);
		cfree(win->_y);
		cfree(win->_firstch);
		return NULL;
	}
# ifdef DEBUG
	fprintf(outf, "MAKENEW: nc = %d\n", nc);
# endif
	win->_cury = win->_curx = 0;
	win->_clear = (nl == LINES && nc == COLS);
	win->_maxy = nl;
	win->_maxx = nc;
	win->_begy = by;
	win->_begx = bx;
	win->_scroll = win->_leave = FALSE;
	for (i = 0; i < nl; i++)
		win->_firstch[i] = win->_lastch[i] = _NOCHANGE;
	if (bx + nc == COLS) {
		win->_flags |= _ENDLINE;
		if (bx == 0 && nl == LINES && by == 0)
			win->_flags |= _FULLWIN;
		if (by + nl == LINES)
			win->_flags |= _SCROLLWIN;
	}
# ifdef DEBUG
	fprintf(outf, "MAKENEW: win->_clear = %d\n", win->_clear);
	fprintf(outf, "MAKENEW: win->_leave = %d\n", win->_leave);
	fprintf(outf, "MAKENEW: win->_scroll = %d\n", win->_scroll);
	fprintf(outf, "MAKENEW: win->_flags = %0.2o\n", win->_flags);
	fprintf(outf, "MAKENEW: win->_maxy = %d\n", win->_maxy);
	fprintf(outf, "MAKENEW: win->_maxx = %d\n", win->_maxx);
	fprintf(outf, "MAKENEW: win->_begy = %d\n", win->_begy);
	fprintf(outf, "MAKENEW: win->_begx = %d\n", win->_begx);
# endif
	return win;
}

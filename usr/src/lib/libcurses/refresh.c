/*
 * make the current screen look like "win" over the area coverd by
 * win.
 *
 * %G% (Berkeley) @(#)refresh.c	1.5
 */

# include	"curses.ext"

# ifdef DEBUG
# define	STATIC
# else
# define	STATIC	static
# endif

STATIC short	ly, lx;

STATIC bool	curwin;

WINDOW	*_win = NULL;

wrefresh(win)
reg WINDOW	*win;
{
	reg short	wy;
	reg int		retval;

	/*
	 * make sure were in visual state
	 */
	if (_endwin) {
		_puts(VS);
		_puts(TI);
		_endwin = FALSE;
	}

	/*
	 * initialize loop parameters
	 */

	ly = curscr->_cury;
	lx = curscr->_curx;
	wy = 0;
	_win = win;
	curwin = (win == curscr);

	if (win->_clear || curscr->_clear || curwin) {
		if ((win->_flags & _FULLWIN) || curscr->_clear) {
			_puts(CL);
			ly = lx = curscr->_curx = curscr->_cury = 0;
			curscr->_clear = FALSE;
			if (!curwin)
				werase(curscr);
			touchwin(win);
		}
		win->_clear = FALSE;
	}
	if (!CA) {
		if (win->_curx != 0)
			putchar('\n');
		if (!curwin)
			werase(curscr);
	}
# ifdef DEBUG
	fprintf(outf, "REFRESH(%0.2o): curwin = %d\n", win, curwin);
	fprintf(outf, "REFRESH:\n\tfirstch\tlastch\n");
# endif
	for (wy = 0; wy < win->_maxy; wy++) {
# ifdef DEBUG
		fprintf(outf, "%d\t%d\t%d\n", wy, win->_firstch[wy], win->_lastch[wy]);
# endif
		if (win->_firstch[wy] != _NOCHANGE)
			if (makech(win, wy) == ERR)
				return ERR;
			else
				win->_firstch[wy] = _NOCHANGE;
	}
	if (win->_leave) {
		curscr->_cury = ly;
		curscr->_curx = lx;
		ly -= win->_begy;
		lx -= win->_begx;
		if (ly >= 0 && ly < win->_maxy && lx >= 0 && lx < win->_maxx) {
			win->_cury = ly;
			win->_curx = lx;
		}
		else
			win->_cury = win->_curx = 0;
	}
	else {
		mvcur(ly, lx, win->_cury + win->_begy, win->_curx + win->_begx);
		curscr->_cury = win->_cury + win->_begy;
		curscr->_curx = win->_curx + win->_begx;
	}
	retval = OK;
ret:
	_win = NULL;
	fflush(stdout);
	return retval;
}

/*
 * make a change on the screen
 */
STATIC
makech(win, wy)
reg WINDOW	*win;
short		wy;
{
	reg char	*nsp, *csp, *ce;
	reg short	wx, lch, y;
	reg int		nlsp, clsp;	/* last space in lines		*/

	wx = win->_firstch[wy];
	y = wy + win->_begy;
	lch = win->_lastch[wy];
	if (curwin)
		csp = " ";
	else
		csp = &curscr->_y[wy + win->_begy][wx + win->_begx];
	nsp = &win->_y[wy][wx];
	if (CE && !curwin) {
		for (ce = &win->_y[wy][win->_maxx - 1]; *ce == ' '; ce--)
			if (ce <= win->_y[wy])
				break;
		nlsp = ce - win->_y[wy];
	}
	if (!curwin)
		ce = CE;
	else
		ce = NULL;
	while (wx <= lch) {
		if (*nsp != *csp) {
			mvcur(ly, lx, y, wx + win->_begx);
# ifdef DEBUG
			fprintf(outf, "MAKECH: 1: wx = %d, lx = %d\n", wx, lx);
# endif	
			ly = y;
			lx = wx + win->_begx;
			while (*nsp != *csp && wx <= lch) {
				if (ce != NULL && wx >= nlsp && *nsp == ' ') {
					/*
					 * check for clear to end-of-line
					 */
					ce = &curscr->_y[ly][COLS - 1];
					while (*ce == ' ')
						if (ce-- <= csp)
							break;
					clsp = ce - curscr->_y[ly] - win->_begx;
# ifdef DEBUG
					fprintf(outf, "MAKECH: clsp = %d, nlsp = %d\n", clsp, nlsp);
# endif
					if (clsp - nlsp >= strlen(CE)
					    && clsp < win->_maxx) {
# ifdef DEBUG
						fprintf(outf, "MAKECH: using CE\n");
# endif
						_puts(CE);
						lx = wx + win->_begx;
						while (wx++ <= clsp)
							*csp++ = ' ';
						goto ret;
					}
					ce = NULL;
				}
				/*
				 * enter/exit standout mode as appropriate
				 */
				if (SO && (*nsp&_STANDOUT) != (curscr->_flags&_STANDOUT)) {
					if (*nsp & _STANDOUT) {
						_puts(SO);
						curscr->_flags |= _STANDOUT;
					}
					else {
						_puts(SE);
						curscr->_flags &= ~_STANDOUT;
					}
				}
				wx++;
				if (wx >= win->_maxx && wy == win->_maxy - 1)
					if (win->_scroll) {
					    if ((win->_flags&(_ENDLINE|_STANDOUT)) == (_ENDLINE|_STANDOUT))
						if (!MS) {
						    _puts(SE);
						    win->_flags &= ~_STANDOUT;
						}
					    if (!curwin)
						putchar((*csp = *nsp) & 0177);
					    else
						putchar(*nsp & 0177);
					    scroll(win);
					    if (win->_flags&_FULLWIN && !curwin)
						scroll(curscr);
					    ly = win->_begy+win->_cury;
					    lx = win->_begx+win->_curx;
					    return OK;
					}
					else if (win->_flags&_SCROLLWIN) {
					    lx = --wx;
					    return ERR;
					}
				if (!curwin)
					putchar((*csp++ = *nsp) & 0177);
				else
					putchar(*nsp & 0177);
				if (UC && (*nsp & _STANDOUT)) {
					putchar('\b');
					_puts(UC);
				}
				nsp++;
			}
			if (!MS && (*nsp & _STANDOUT)  && (*csp & _STANDOUT)) {
				_puts(SE);
				win->_flags &= ~_STANDOUT;
			}
# ifdef DEBUG
			fprintf(outf, "MAKECH: 2: wx = %d, lx = %d\n", wx, lx);
# endif	
			if (lx == wx + win->_begx)	/* if no change */
				break;
			lx = wx + win->_begx;
		}
		else if (wx < lch)
			while (*nsp == *csp) {
				nsp++;
				if (!curwin)
					csp++;
				++wx;
			}
		else
			break;
# ifdef DEBUG
		fprintf(outf, "MAKECH: 3: wx = %d, lx = %d\n", wx, lx);
# endif	
	}
ret:
	if ((win->_flags & _STANDOUT) && !MS) {
		_puts(SE);
		win->_flags &= ~_STANDOUT;
	}
	return OK;
}

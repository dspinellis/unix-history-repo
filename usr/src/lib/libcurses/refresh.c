# include	"curses.ext"

# ifndef DEBUG
static short	ly, lx;
# else
short		ly, lx;
# endif

wrefresh(win)
reg WINDOW	*win;
{
	reg short	wy;

	/*
	 * make sure were in visual state
	 */
	if (_endwin) {
		_puts(VS);
		_puts(TI);
		_endwin = FALSE;
	}
	if (win->_clear || curscr->_clear) {
		if ((win->_flags & _FULLWIN) || curscr->_clear) {
			_puts(CL);
			curscr->_curx = curscr->_cury = 0;
			curscr->_clear = FALSE;
			werase(curscr);
		}
		win->_clear = FALSE;
	}
	if (!CA) {
		if (win->_curx != 0)
			putchar('\n');
		werase(curscr);
	}
# ifdef DEBUG
	fprintf(outf, "REFRESH:\n\tfirstch\tlastch\n");
# endif
	ly = curscr->_cury;
	lx = curscr->_curx;
	wy = 0;
	for (wy = 0; wy < win->_maxy; wy++) {
# ifdef DEBUG
		fprintf(outf, "%d\t%d\t%d\n", wy, win->_firstch[wy], win->_lastch[wy]);
# endif
		if (win->_firstch[wy] != _NOCHANGE)
			if (makech(win, wy) == ERR)
			    return ERR;
			else
			    win->_firstch[wy] = win->_firstch[wy] = _NOCHANGE;
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
	fflush(stdout);
	return OK;
}

/*
 * make a change on the screen
 */
# ifndef DEBUG
static
# endif
makech(win, wy)
reg WINDOW	*win;
short		wy;
{
	reg char	*nsp, *csp, *sp;
	reg short	wx, lch, y;
	reg int		nlsp, clsp;	/* last space in lines		*/

	wx = win->_firstch[wy];
	y = wy + win->_begy;
	lch = win->_lastch[wy];
	csp = &curscr->_y[wy + win->_begy][wx + win->_begx];
	nsp = &win->_y[wy][wx];
	if (CE) {
		for (sp = &win->_y[wy][win->_maxx - 1]; *sp == ' '; sp--)
			if (sp <= win->_y[wy])
				break;
		nlsp = sp - win->_y[wy];
	}
	sp = CE;
	while (wx <= lch) {
		if (*nsp != *csp) {
			mvcur(ly, lx, y, wx + win->_begx);
# ifdef DEBUG
			fprintf(outf, "MAKECH: 1: wx = %d, lx = %d\n", wx, lx);
# endif	
			ly = y;
			lx = wx + win->_begx;
			while (*nsp != *csp && wx <= lch) {
				if (sp && wx >= nlsp && *nsp == ' ') {
					/*
					 * check for clear to end-of-line
					 */
					sp = &curscr->_y[ly][COLS - 1];
					while (*sp == ' ')
						if (sp-- <= csp)
							break;
					clsp = sp - curscr->_y[ly] - win->_begx;
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
					sp = NULL;
				}
				/*
				 * enter/exit standout mode as appropriate
				 */
				if ((*nsp&_STANDOUT) != (curscr->_flags&_STANDOUT)) {
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
				if (wx >= win->_maxx && wy == win->_maxy)
						if (win->_scroll) {
						    putchar((*csp = *nsp) & 0177);
						    scroll(win);
						    if (win->_flags&_FULLWIN)
							    scroll(curscr);
						    ly = win->_begy+win->_cury;
						    lx = win->_begx+win->_curx;
						    return OK;
						}
						else if (win->_flags&_SCROLLWIN) {
						    lx = --wx;
						    return ERR;
						}
				putchar((*csp++ = *nsp++) & 0177);
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
				nsp++, csp++;
				++wx;
			}
		else
			break;
# ifdef DEBUG
		fprintf(outf, "MAKECH: 3: wx = %d, lx = %d\n", wx, lx);
# endif	
	}
ret:
	return OK;
}

# include	"curses.ext"

extern char	*getenv();

/*
 *	This routine initializes the current and standard screen.
 *
 * %G% (Berkeley) @(#)initscr.c	1.1
 */
WINDOW *
initscr() {

	reg char	*sp;

# ifdef DEBUG
	fprintf(outf, "INITSCR()\n");
# endif
	if (!My_term && isatty(2)) {
		_tty_ch = 2;
		gettmode();
		if ((sp = getenv("TERM")) == NULL)
			sp = Def_term;
		setterm(sp);
# ifdef DEBUG
		fprintf(outf, "INITSCR: term = %s\n", sp);
# endif
	}
	else
		setterm(Def_term);
	_puts(TI);
	_puts(VS);
	if (curscr != NULL) {
# ifdef DEBUG
		fprintf(outf, "INITSCR: curscr = 0%o\n", curscr);
# endif
		delwin(curscr);
	}
# ifdef DEBUG
	fprintf(outf, "LINES = %d, COLS = %d\n", LINES, COLS);
# endif
	if ((curscr=newwin(LINES, COLS, 0, 0)) == ERR)
		return ERR;
	curscr->_clear = TRUE;
	if (stdscr != NULL) {
# ifdef DEBUG
		fprintf(outf, "INITSCR: stdscr = 0%o\n", stdscr);
# endif
		delwin(stdscr);
	}
	stdscr = newwin(LINES, COLS, 0, 0);
	return stdscr;
}

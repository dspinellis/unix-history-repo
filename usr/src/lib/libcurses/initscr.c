/*
 * Copyright (c) 1981 Regents of the University of California.
 * All rights reserved.
 *
 * Redistribution and use in source and binary forms are permitted
 * provided that this notice is preserved and that due credit is given
 * to the University of California at Berkeley. The name of the University
 * may not be used to endorse or promote products derived from this
 * software without specific prior written permission. This software
 * is provided ``as is'' without express or implied warranty.
 */

#ifndef lint
static char sccsid[] = "@(#)initscr.c	5.2 (Berkeley) %G%";
#endif /* not lint */

# include	"curses.ext"
# include	<signal.h>

extern char	*getenv();

/*
 *	This routine initializes the current and standard screen.
 *
 */
WINDOW *
initscr() {

	reg char	*sp;
	int		tstp();
	int 		nfd;

# ifdef DEBUG
	fprintf(outf, "INITSCR()\n");
# endif
	if (My_term)
		setterm(Def_term);
	else {
		for (_tty_ch = 0; _tty_ch < nfd; _tty_ch++)
			if (isatty(_tty_ch))
				break;
		gettmode();
		if ((sp = getenv("TERM")) == NULL)
			sp = Def_term;
		setterm(sp);
# ifdef DEBUG
		fprintf(outf, "INITSCR: term = %s\n", sp);
# endif
	}
	_puts(TI);
	_puts(VS);
# ifdef SIGTSTP
	signal(SIGTSTP, tstp);
# endif
	if (curscr != NULL) {
# ifdef DEBUG
		fprintf(outf, "INITSCR: curscr = 0%o\n", curscr);
# endif
		delwin(curscr);
	}
# ifdef DEBUG
	fprintf(outf, "LINES = %d, COLS = %d\n", LINES, COLS);
# endif
	if ((curscr = newwin(LINES, COLS, 0, 0)) == ERR)
		return ERR;
	clearok(curscr, TRUE);
	curscr->_flags &= ~_FULLLINE;
	if (stdscr != NULL) {
# ifdef DEBUG
		fprintf(outf, "INITSCR: stdscr = 0%o\n", stdscr);
# endif
		delwin(stdscr);
	}
	stdscr = newwin(LINES, COLS, 0, 0);
	return stdscr;
}

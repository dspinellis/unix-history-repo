/*
 * Copyright (c) 1981 Regents of the University of California.
 * All rights reserved.
 *
 * Redistribution and use in source and binary forms are permitted
 * provided that: (1) source distributions retain this entire copyright
 * notice and comment, and (2) distributions including binaries display
 * the following acknowledgement:  ``This product includes software
 * developed by the University of California, Berkeley and its contributors''
 * in the documentation or other materials provided with the distribution
 * and in all advertising materials mentioning features or use of this
 * software. Neither the name of the University nor the names of its
 * contributors may be used to endorse or promote products derived
 * from this software without specific prior written permission.
 * THIS SOFTWARE IS PROVIDED ``AS IS'' AND WITHOUT ANY EXPRESS OR
 * IMPLIED WARRANTIES, INCLUDING, WITHOUT LIMITATION, THE IMPLIED
 * WARRANTIES OF MERCHANTABILITY AND FITNESS FOR A PARTICULAR PURPOSE.
 */

#ifndef lint
static char sccsid[] = "@(#)initscr.c	5.5 (Berkeley) 6/1/90";
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

# ifdef DEBUG
	fprintf(outf, "INITSCR()\n");
# endif
	if (My_term)
		setterm(Def_term);
	else {
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

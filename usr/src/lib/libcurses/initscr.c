/*
 * Copyright (c) 1981 Regents of the University of California.
 * All rights reserved.
 *
 * %sccs.include.redist.c%
 */

#ifndef lint
static char sccsid[] = "@(#)initscr.c	5.9 (Berkeley) %G%";
#endif	/* not lint */

#include <curses.h>
#include <signal.h>
#include <stdlib.h>

/*
 * initscr --
 *	Initialize the current and standard screen.
 */
WINDOW *
initscr()
{
	register char *sp;

#ifdef DEBUG
	__TRACE("initscr\n");
#endif
	if (My_term) {
		if (setterm(Def_term) == ERR)
			return (NULL);
	} else {
		gettmode();
		if ((sp = getenv("TERM")) == NULL)
			sp = Def_term;
		if (setterm(sp) == ERR)
			return (NULL);
#ifdef DEBUG
		__TRACE("initscr: term = %s\n", sp);
#endif
	}
	/* Need either homing or cursor motion for refreshes */
	if (!HO && !CM) 
		return(NULL);
	tputs(TI, 0, __cputchar);
	tputs(VS, 0, __cputchar);
	(void)signal(SIGTSTP, tstp);
	if (curscr != NULL) {
#ifdef DEBUG
		__TRACE("initscr: curscr = 0%o\n", curscr);
#endif
		delwin(curscr);
	}
#ifdef DEBUG
	__TRACE("initscr: LINES = %d, COLS = %d\n", LINES, COLS);
#endif
	if ((curscr = newwin(LINES, COLS, 0, 0)) == ERR)
		return (NULL);
	clearok(curscr, 1);
	curscr->flags &= ~__FULLLINE;
	if (stdscr != NULL) {
#ifdef DEBUG
		__TRACE("initscr: stdscr = 0%o\n", stdscr);
#endif
		delwin(stdscr);
	}
	return(stdscr = newwin(LINES, COLS, 0, 0));
}


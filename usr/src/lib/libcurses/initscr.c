/*
 * Copyright (c) 1981 Regents of the University of California.
 * All rights reserved.
 *
 * %sccs.include.redist.c%
 */

#ifndef lint
static char sccsid[] = "@(#)initscr.c	5.10 (Berkeley) %G%";
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
	__echoit = 1;
        __pfast = __rawmode = __noqch = 0;

	if (gettmode() == ERR)
		return (NULL);

	/*
	 * If My_term is set, or can't find a terminal in the environment,
	 * use Def_term.
	 */
	if (My_term || (sp = getenv("TERM")) == NULL)
		sp = Def_term;
	if (setterm(sp) == ERR)
		return (NULL);

	/* Need either homing or cursor motion for refreshes */
	if (!HO && !CM) 
		return (NULL);

	tputs(TI, 0, __cputchar);
	tputs(VS, 0, __cputchar);

	if (curscr != NULL)
		delwin(curscr);
	if ((curscr = newwin(LINES, COLS, 0, 0)) == ERR)
		return (NULL);
	clearok(curscr, 1);

	if (stdscr != NULL)
		delwin(stdscr);
	if ((stdscr = newwin(LINES, COLS, 0, 0)) == ERR) {
		delwin(curscr);
		return (NULL);
	}

	(void)signal(SIGTSTP, tstp);

#ifdef DEBUG
	__TRACE("initscr: LINES = %d, COLS = %d\n", LINES, COLS);
#endif
	return (stdscr);
}


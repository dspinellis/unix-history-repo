/*
 * Copyright (c) 1980 Regents of the University of California.
 * All rights reserved.  The Berkeley software License Agreement
 * specifies the terms and conditions for redistribution.
 */

#ifndef lint
static char sccsid[] = "@(#)printw.c	5.2 (Berkeley) %G%";
#endif not lint

/*
 * printw and friends
 *
 */

# include	"curses.ext"

/*
 *	This routine implements a printf on the standard screen.
 */
printw(fmt, args)
char	*fmt;
int	args; {

	char	buf[512];

	(void) vsprintf(buf, fmt, &args);
	return waddstr(stdscr, buf);
}

/*
 *	This routine implements a printf on the given window.
 */
wprintw(win, fmt, args)
WINDOW	*win;
char	*fmt;
int	args; {

	char	buf[512];

	(void) vsprintf(buf, fmt, &args);
	return waddstr(win, buf);
}

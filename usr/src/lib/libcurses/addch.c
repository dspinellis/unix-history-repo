/*
 * Copyright (c) 1981 Regents of the University of California.
 * All rights reserved.
 *
 * %sccs.include.redist.c%
 */

#ifndef lint
static char sccsid[] = "@(#)addch.c	5.6 (Berkeley) %G%";
#endif	/* not lint */

#include <curses.h>

/*
 * waddch --
 *	Add the character to the current position in the given window.
 *
 */
int
waddch(win, c)
	WINDOW *win;
	int ch;
{
	static char buf[2];

	buf[0] = ch;
	return (waddbytes(win, buf, 1));
}

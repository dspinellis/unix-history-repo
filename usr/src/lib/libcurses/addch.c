/*
 * Copyright (c) 1981 Regents of the University of California.
 * All rights reserved.
 *
 * %sccs.include.redist.c%
 */

#ifndef lint
static char sccsid[] = "@(#)addch.c	5.8 (Berkeley) %G%";
#endif	/* not lint */

#include <curses.h>

/*
 * waddch --
 *	Add the character to the current position in the given window.
 *
 */

int
waddch(win, ch)
	WINDOW *win;
	int ch;
{
	__waddch(win, ch, 0);
}

int
__waddch(win, ch, so)
	WINDOW *win;
	int ch;
	int so;
{
	static char buf[2];

	buf[0] = ch;
	return (__waddbytes(win, buf, 1, so));
}

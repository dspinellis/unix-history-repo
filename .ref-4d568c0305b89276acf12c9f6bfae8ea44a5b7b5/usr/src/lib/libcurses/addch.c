/*
 * Copyright (c) 1981, 1993
 *	The Regents of the University of California.  All rights reserved.
 *
 * %sccs.include.redist.c%
 */

#ifndef lint
static char sccsid[] = "@(#)addch.c	8.1 (Berkeley) %G%";
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
	__LDATA buf;

	buf.ch = ch;
	buf.attr = 0;
	return (__waddch(win, &buf));
}

int
__waddch(win, dp)
	WINDOW *win;
	__LDATA *dp;
{
	char buf[2];

	buf[0] = dp->ch;
	return (__waddbytes(win, buf, 1, dp->attr & __STANDOUT));
}

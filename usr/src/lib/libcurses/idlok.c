/*
 * Copyright (c) 1980 Regents of the University of California.
 * All rights reserved.  The Berkeley software License Agreement
 * specifies the terms and conditions for redistribution.
 */

#ifndef lint
static char sccsid[] = "@(#)idlok.c	5.1 (Berkeley) %G%";
#endif not lint

# include	"curses.ext"

/*
 * idlok:
 *	Turn on and off using insert/deleteln sequences for the given
 *	window.
 *
 */
idlok(win, bf)
register WINDOW	*win;
bool		bf;
{
	if (bf)
		win->_flags |= _IDLINE;
	else
		win->_flags &= ~_IDLINE;
}

/*
 * Copyright (c) 1981 Regents of the University of California.
 * All rights reserved.
 *
 * %sccs.include.redist.c%
 */

#ifndef lint
static char sccsid[] = "@(#)idlok.c	5.5 (Berkeley) %G%";
#endif	/* not lint */

#include <curses.h>

/*
 * idlok --
 *	Turn on and off using insert/deleteln sequences for the
 *	given window.
 */
void
idlok(win, bf)
	WINDOW *win;
	int bf;
{
	if (bf)
		win->_flags |= _IDLINE;
	else
		win->_flags &= ~_IDLINE;
}

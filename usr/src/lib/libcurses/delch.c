/*
 * Copyright (c) 1981 Regents of the University of California.
 * All rights reserved.
 *
 * %sccs.include.redist.c%
 */

#ifndef lint
static char sccsid[] = "@(#)delch.c	5.7 (Berkeley) %G%";
#endif	/* not lint */

#include <curses.h>

/*
 * wdelch --
 *	Do an insert-char on the line, leaving (cury, curx) unchanged.
 */
int
wdelch(win)
	register WINDOW *win;
{
	register char *end, *temp1, *temp2;

	end = &win->lines[win->cury]->line[win->maxx - 1];
	temp1 = &win->lines[win->cury]->line[win->curx];
	temp2 = temp1 + 1;
	while (temp1 < end)
		*temp1++ = *temp2++;
	*temp1 = ' ';
	touchline(win, win->cury, win->curx, win->maxx - 1);
	return (OK);
}

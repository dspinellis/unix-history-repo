/*
 * Copyright (c) 1981 Regents of the University of California.
 * All rights reserved.
 *
 * %sccs.include.redist.c%
 */

#ifndef lint
static char sccsid[] = "@(#)insch.c	5.6 (Berkeley) %G%";
#endif	/* not lint */

#include <curses.h>

/*
 * winsch --
 *	Do an insert-char on the line, leaving (_cury,_curx) unchanged.
 */
int
winsch(win, ch)
	register WINDOW *win;
	int ch;
{

	register char *end, *temp1, *temp2;

	end = &win->lines[win->cury]->line[win->curx];
	temp1 = &win->lines[win->cury]->line[win->maxx - 1];
	temp2 = temp1 - 1;
	while (temp1 > end)
		*temp1-- = *temp2--;
	*temp1 = ch;
	touchline(win, win->cury, win->curx, win->maxx - 1);
	if (win->cury == LINES - 1 && 
	    win->lines[LINES - 1]->line[COLS - 1] != ' ')
		if (win->flags & __SCROLLOK) {
			wrefresh(win);
			scroll(win);
			win->cury--;
		} else
			return (ERR);
	return (OK);
}

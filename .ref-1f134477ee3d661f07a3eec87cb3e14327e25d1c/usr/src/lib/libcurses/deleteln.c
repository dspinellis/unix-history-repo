/*
 * Copyright (c) 1981, 1993
 *	The Regents of the University of California.  All rights reserved.
 *
 * %sccs.include.redist.c%
 */

#ifndef lint
static char sccsid[] = "@(#)deleteln.c	8.1 (Berkeley) %G%";
#endif	/* not lint */

#include <curses.h>
#include <string.h>

/*
 * wdeleteln --
 *	Delete a line from the screen.  It leaves (cury, curx) unchanged.
 */
int
wdeleteln(win)
	register WINDOW *win;
{
	register int y, i;
	register __LINE *temp;

#ifdef DEBUG
	__CTRACE("deleteln: (%0.2o)\n", win);
#endif
	temp = win->lines[win->cury];
	for (y = win->cury; y < win->maxy - 1; y++) {
		win->lines[y]->flags &= ~__ISPASTEOL;
		win->lines[y + 1]->flags &= ~__ISPASTEOL;
		if (win->orig == NULL)
			win->lines[y] = win->lines[y + 1];
		else
			(void) memcpy(win->lines[y]->line, 
			    win->lines[y + 1]->line, 
			    win->maxx * __LDATASIZE);
		__touchline(win, y, 0, win->maxx - 1, 0);
	}

	if (win->orig == NULL)
		win->lines[y] = temp;
	else
		temp = win->lines[y];

	for(i = 0; i < win->maxx; i++) {
		temp->line[i].ch = ' ';
		temp->line[i].attr = 0;
	}
	__touchline(win, y, 0, win->maxx - 1, 0);
	if (win->orig == NULL)
		__id_subwins(win);
	return (OK);
}

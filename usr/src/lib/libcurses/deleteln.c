/*
 * Copyright (c) 1981 Regents of the University of California.
 * All rights reserved.
 *
 * %sccs.include.redist.c%
 */

#ifndef lint
static char sccsid[] = "@(#)deleteln.c	5.8 (Berkeley) %G%";
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
	register int y;
	register LINE *temp;

#ifdef DEBUG
	__TRACE("deleteln: (%0.2o)\n", win);
#endif
	temp = win->lines[win->cury];
	for (y = win->cury; y < win->maxy - 1; y++) {
		if (win->orig == NULL)
			win->lines[y] = win->lines[y + 1];
		else
			bcopy(win->lines[y + 1]->line, win->lines[y]->line, 
			      win->maxx);
		touchline(win, y, 0, win->maxx - 1);
	}

	if (win->orig == NULL)
		win->lines[y] = temp;
	else
		temp = win->lines[y];

	(void)memset(temp->line, ' ', &temp->line[win->maxx] - temp->line);
	touchline(win, y, 0, win->maxx - 1);
	if (win->orig == NULL)
		__id_subwins(win);
	return (OK);
}

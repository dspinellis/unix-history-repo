/*
 * Copyright (c) 1981 Regents of the University of California.
 * All rights reserved.
 *
 * %sccs.include.redist.c%
 */

#ifndef lint
static char sccsid[] = "@(#)scroll.c	8.1 (Berkeley) %G%";
#endif /* not lint */

#include <curses.h>
#include <termios.h>

/*
 * scroll --
 *	Scroll the window up a line.
 */
int
scroll(win)
	register WINDOW *win;
{
	register int oy, ox;

#ifdef DEBUG
	__CTRACE("scroll: (%0.2o)\n", win);
#endif

	if (!(win->flags & __SCROLLOK))
		return (ERR);

	getyx(win, oy, ox);
	wmove(win, 0, 0);
	wdeleteln(win);
	wmove(win, oy, ox);

	if (win == curscr) {
		putchar('\n');
		if (__baset.c_oflag & ONLCR)
			win->curx = 0;
#ifdef DEBUG
		__CTRACE("scroll: win == curscr\n");
#endif
	}
	return (OK);
}

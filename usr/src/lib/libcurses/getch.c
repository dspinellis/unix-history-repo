/*
 * Copyright (c) 1981 Regents of the University of California.
 * All rights reserved.
 *
 * %sccs.include.redist.c%
 */

#ifndef lint
static char sccsid[] = "@(#)getch.c	5.8 (Berkeley) %G%";
#endif	/* not lint */

#include <curses.h>

/*
 * wgetch --
 *	Read in a character from the window.
 */
int
wgetch(win)
	register WINDOW *win;
{
	register int inp, weset;

	if (!(win->flags & __SCROLLOK) && (win->flags & __FULLWIN)
	    && win->curx == win->maxx - 1 && win->cury == win->maxy - 1)
		return (ERR);
#ifdef DEBUG
	__TRACE("wgetch: __echoit = %d, __rawmode = %d\n",
	    __echoit, __rawmode);
#endif
	if (__echoit && !__rawmode) {
		cbreak();
		weset = 1;
	} else
		weset = 0;

	inp = getchar();
#ifdef DEBUG
	__TRACE("wgetch got '%s'\n", unctrl(inp));
#endif
	if (__echoit) {
		mvwaddch(curscr,
		    win->cury + win->begy, win->curx + win->begx, inp);
		waddch(win, inp);
	}
	if (weset)
		nocbreak();
	return (inp);
}

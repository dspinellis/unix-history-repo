/*
 * Copyright (c) 1987 Regents of the University of California.
 * All rights reserved.
 *
 * %sccs.include.redist.c%
 */

#ifndef lint
static char sccsid[] = "@(#)addbytes.c	5.9 (Berkeley) %G%";
#endif	/* not lint */

#include <curses.h>
#include <termios.h>

#define	SYNCH_IN	{y = win->cury; x = win->curx;}
#define	SYNCH_OUT	{win->cury = y; win->curx = x;}

/*
 * waddbytes --
 *	Add the character to the current position in the given window.
 */
int
waddbytes(win, bytes, count)
	register WINDOW *win;
	register char *bytes;
	register int count;
{
	static char blanks[] = "        ";
	register int c, newx, x, y;
	LINE *lp;

	SYNCH_IN;

#ifdef DEBUG
	__TRACE("ADDBYTES('%c') at (%d, %d)\n", c, y, x);
#endif
	while (count--) {
		c = *bytes++;
		switch (c) {
		case '\t':
			SYNCH_OUT;
			if (waddbytes(win, blanks, 8 - (x % 8)) == ERR)
				return (ERR);
			SYNCH_IN;
			break;

		default:
#ifdef DEBUG
	__TRACE("ADDBYTES: 1: y = %d, x = %d, firstch = %d, lastch = %d\n",
	    y, x, win->lines[y]->firstch, win->lines[y]->lastch);
#endif
			if (win->flags & __WSTANDOUT)
				c |= __STANDOUT;
#ifdef DEBUG
	__TRACE("ADDBYTES(%0.2o, %d, %d)\n", win, y, x);
#endif
			lp = win->lines[y];
			
			if (lp->line[x] != c) {
				newx = x + win->ch_off;
				if (!(lp->flags & __ISDIRTY)) {
					lp->flags |= __ISDIRTY;
					lp->firstch = lp->lastch = newx;
				}
				else if (newx < lp->firstch)
					lp->firstch = newx;
				else if (newx > lp->lastch)
					lp->lastch = newx;
#ifdef DEBUG
	__TRACE("ADDBYTES: change gives f/l: %d/%d [%d/%d]\n",
	    lp->firstch, lp->lastch,
	    lp->firstch - win->ch_off,
	    lp->lastch - win->ch_off);
#endif
			}
			lp->line[x] = c;
			if (++x >= win->maxx) {
				x = 0;
newline:			if (++y >= win->maxy) 
					if (win->flags & __SCROLLOK) {
						SYNCH_OUT;
						scroll(win);
						SYNCH_IN;
						--y;
					} else
						return (ERR);
			}
#ifdef DEBUG
	__TRACE("ADDBYTES: 2: y = %d, x = %d, firstch = %d, lastch = %d\n",
	    y, x, win->lines[y]->firstch, win->lines[y]->lastch);
#endif
			break;
		case '\n':
			SYNCH_OUT;
			wclrtoeol(win);
			SYNCH_IN;
			if (origtermio.c_oflag & ONLCR)
				x = 0;
			goto newline;
		case '\r':
			x = 0;
			break;
		case '\b':
			if (--x < 0)
				x = 0;
			break;
		}
	}
	SYNCH_OUT;
	return (OK);
}

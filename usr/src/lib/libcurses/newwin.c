/*
 * Copyright (c) 1981 Regents of the University of California.
 * All rights reserved.
 *
 * %sccs.include.redist.c%
 */

#ifndef lint
static char sccsid[] = "@(#)newwin.c	5.8 (Berkeley) %G%";
#endif	/* not lint */

#include <curses.h>
#include <stdlib.h>

#undef	nl		/* Don't need it here, and it interferes. */

static WINDOW 	*makenew __P((int, int, int, int));
static void	 __swflags __P((WINDOW *));

void	 __set_subwin __P((WINDOW *, WINDOW *));

/*
 * newwin --
 *	Allocate space for and set up defaults for a new window.
 */
WINDOW *
newwin(nl, nc, by, bx)
	register int nl, nc, by, bx;
{
	register WINDOW *win;
	register LINE *lp;
	register int  i, j;
	register char *sp;

	if (nl == 0)
		nl = LINES - by;
	if (nc == 0)
		nc = COLS - bx;

	if ((win = makenew(nl, nc, by, bx)) == NULL)
		return (NULL);

	win->nextp = win;
	win->ch_off = 0;

#ifdef DEBUG
	__TRACE("newwin: win->ch_off = %d\n", win->ch_off);
#endif

	for (lp = win->topline, i = 0; i < nl; i++, lp = lp->next) {
		lp->flags = 0;
		lp->flags &= ~__ISDIRTY;
		lp->flags &= ~__ISPASTEOL;
		for (sp = lp->line; sp < lp->line + nc; sp++)
			*sp = ' ';
		lp->hash = __hash(lp->line, nc);
	}

	return (win);
}

WINDOW *
subwin(orig, nl, nc, by, bx)
	register WINDOW *orig;
	register int by, bx, nl, nc;
{
	register WINDOW *win;

	/* Make sure window fits inside the original one. */
#ifdef	DEBUG
	__TRACE("subwin: (%0.2o, %d, %d, %d, %d)\n", orig, nl, nc, by, bx);
#endif
	if (by < orig->begy || bx < orig->begx
	    || by + nl > orig->maxy + orig->begy
	    || bx + nc > orig->maxx + orig->begx)
		return (NULL);
	if (nl == 0)
		nl = orig->maxy + orig->begy - by;
	if (nc == 0)
		nc = orig->maxx + orig->begx - bx;
	if ((win = makenew(nl, nc, by, bx)) == NULL)
		return (NULL);
	win->nextp = orig->nextp;
	orig->nextp = win;
	win->orig = orig;
	__set_subwin(orig, win);
	return (win);
}

/*
 * This code is shared with mvwin().
 */
void
__set_subwin(orig, win)
	register WINDOW *orig, *win;
{
	register int j, k, ocnt, cnt;
	register LINE *lp, *olp;

	j = win->begy - orig->begy;
	k = win->begx - orig->begx;
	win->ch_off = k;
#ifdef DEBUG
	__TRACE("__set_subwin: win->ch_off = %d\n", win->ch_off);
#endif

	lp = win->topline;
	olp = orig->topline; 
	ocnt = 0;
	for (ocnt = 0; ocnt < orig->maxy && cnt < win->maxy; ocnt++) {
		if (ocnt >= j) {
			lp->firstch = olp->firstch;
			lp->lastch = olp->lastch;
			lp->line = &olp->line[k];
			lp = lp->next;
			lp->flags = olp->flags;
			cnt++;
		}
		olp = olp->next;
	}
}

/*
 * makenew --
 *	Set up a window buffer and returns a pointer to it.
 */
static WINDOW *
makenew(nl, nc, by, bx)
	register int by, bx, nl, nc;
{
	register WINDOW *win;
	register LINE *cur, *prev;
	int i;

#ifdef	DEBUG
	__TRACE("makenew: (%d, %d, %d, %d)\n", nl, nc, by, bx);
#endif
	if ((win = malloc(sizeof(*win))) == NULL)
		return (NULL);
#ifdef DEBUG
	__TRACE("makenew: nl = %d\n", nl);
#endif

	/*
	 * Allocate structure of lines in a window.
         */
	if ((win->topline = malloc (nl * sizeof (LINE))) == NULL) {
		free (win);
		return NULL;
	}

	/*
	 * Allocate window space in one chunk.
	 */
	if ((win->wspace = malloc(nc * nl)) == NULL) {
		free(win->topline);
		free(win);
		return NULL;
	}
	/* 
 	 * Link up the lines, set up line pointer array and point line pointers
	 * to the line space.
         */
	if ((win->lines = malloc (nl * sizeof (LINE *))) == NULL) {
		free(win->wspace);
		free(win->topline);
		free(win);
		return NULL;
	}
	prev = &win->topline[nl - 1];
	cur = win->topline;
	for (i = 1; i <= nl; i++, prev = cur, cur = cur->next) {
		cur->next = &win->topline[i % nl];
		cur->prev = prev;
		cur->line = &win->wspace[(i - 1) * nc];
		win->lines[i - 1] = cur;
	}

#ifdef DEBUG
	__TRACE("makenew: nc = %d\n", nc);
#endif
	win->cury = win->curx = 0;
	win->maxy = nl;
	win->maxx = nc;

	win->begy = by;
	win->begx = bx;
	win->flags = 0;
	__swflags(win);
#ifdef DEBUG
	__TRACE("makenew: win->flags = %0.2o\n", win->flags);
	__TRACE("makenew: win->maxy = %d\n", win->maxy);
	__TRACE("makenew: win->maxx = %d\n", win->maxx);
	__TRACE("makenew: win->begy = %d\n", win->begy);
	__TRACE("makenew: win->begx = %d\n", win->begx);
#endif
	return (win);
}

static void
__swflags(win)
	register WINDOW *win;
{
	win->flags &= 
	    ~(__ENDLINE | __FULLLINE | __FULLWIN | __SCROLLWIN | __LEAVEOK);
	if (win->begx + win->maxx == COLS) {
		win->flags |= __ENDLINE;
		if (win->begx == 0) {
			if (AL && DL)
				win->flags |= __FULLLINE;
			if (win->maxy == LINES && win->begy == 0)
				win->flags |= __FULLWIN;
		}
		if (win->begy + win->maxy == LINES)
			win->flags |= __SCROLLWIN;
	}
}

/*
 * Copyright (c) 1983 Regents of the University of California.
 * All rights reserved.
 *
 * Redistribution and use in source and binary forms are permitted
 * provided that the above copyright notice and this paragraph are
 * duplicated in all such forms and that any documentation,
 * advertising materials, and other materials related to such
 * distribution and use acknowledge that the software was developed
 * by the University of California, Berkeley.  The name of the
 * University may not be used to endorse or promote products derived
 * from this software without specific prior written permission.
 * THIS SOFTWARE IS PROVIDED ``AS IS'' AND WITHOUT ANY EXPRESS OR
 * IMPLIED WARRANTIES, INCLUDING, WITHOUT LIMITATION, THE IMPLIED
 * WARRANTIES OF MERCHANTIBILITY AND FITNESS FOR A PARTICULAR PURPOSE.
 */

#ifndef lint
static char sccsid[] = "@(#)wwscroll.c	3.20 (Berkeley) %G%";
#endif /* not lint */

#include "ww.h"
#include "tt.h"

wwscroll(w, n)
register struct ww *w;
int n;
{
	register dir;
	register top;

	if (n == 0)
		return;
	dir = n < 0 ? -1 : 1;
	top = w->ww_b.t - n;
	if (top > w->ww_w.t)
		top = w->ww_w.t;
	else if (top + w->ww_b.nr < w->ww_w.b)
		top = w->ww_w.b - w->ww_b.nr;
	n = abs(top - w->ww_b.t);
	if (n < w->ww_i.nr) {
		while (--n >= 0) {
			(void) wwscroll1(w, w->ww_i.t, w->ww_i.b, dir, 0);
			w->ww_buf += dir;
			w->ww_b.t -= dir;
			w->ww_b.b -= dir;
		}
	} else {
		w->ww_buf -= top - w->ww_b.t;
		w->ww_b.t = top;
		w->ww_b.b = top + w->ww_b.nr;
		wwredrawwin(w);
	}
}

/*
 * Scroll one line, between 'row1' and 'row2', in direction 'dir'.
 * Don't adjust ww_scroll.
 * And don't redraw 'leaveit' lines.
 */
wwscroll1(w, row1, row2, dir, leaveit)
register struct ww *w;
int row1, row2, dir;
int leaveit;
{
	register i;
	int row1x, row2x;
	int nvis;
	int nvismax;
	int scrolled = 0;
	int (*scroll_func)();

	/*
	 * See how many lines on the screen are affected.
	 * And calculate row1x, row2x, and left at the same time.
	 */
	for (i = row1; i < row2 && w->ww_nvis[i] == 0; i++)
		;
	if (i >= row2)			/* can't do any fancy stuff */
		goto out;
	row1x = i;
	for (i = row2 - 1; i >= row1 && w->ww_nvis[i] == 0; i--)
		;
	if (i <= row1x)
		goto out;		/* just one line is easy */
	row2x = i + 1;

	/*
	 * See how much of this window is visible.
	 */
	nvismax = wwncol * (row2x - row1x);
	nvis = 0;
	for (i = row1x; i < row2x; i++)
		nvis += w->ww_nvis[i];

	/*
	 * If it's a good idea to scroll and the terminal can, then do it.
	 * We handle retain (da and db) by putting the burden on scrolling up,
	 * which is the less common operation.  It must ensure that
	 * text is not pushed below the screen, so scrolling down doesn't
	 * have to worry about it.
	 */
	if (nvis < nvismax / 2)
		goto no_scroll;		/* not worth it */
	/*
	 * Try scrolling region (or scrolling the whole screen) first.
	 * Can we assume "sr" doesn't push text below the screen
	 * so we don't have to worry about retain below?
	 * What about scrolling down with a newline?  It probably does
	 * push text above (with da).  Scrolling up would then have
	 * to take care of that.
	 * It's easy to be fool proof, but that slows things down.
	 * The current solution is to disallow tt_scroll_up if da or db is true
	 * but cs (scrolling region) is not.  Again, we sacrifice scrolling
	 * up in favor of scrolling down.  The idea is having scrolling regions
	 * probably means we can scroll (even the whole screen) with impunity.
	 * This lets us work efficiently on simple terminals (use newline
	 * on the bottom to scroll), on any terminal without retain, and
	 * on vt100 style scrolling regions (I think).
	 */
	if (scroll_func = dir > 0 ? tt.tt_scroll_down : tt.tt_scroll_up) {
		if (tt.tt_scroll_top != row1x || tt.tt_scroll_bot != row2x - 1)
			if (tt.tt_setscroll == 0)
				scroll_func = 0;
			else
				(*tt.tt_setscroll)(row1x, row2x - 1);
		if (scroll_func) {
			(*scroll_func)();
			goto did_scroll;
		}
	}
	/*
	 * Try insert/delete line.
	 * Don't worry about retain when scrolling down,
	 * but do worry when scrolling up, for hp2621.
	 */
	if (tt.tt_delline == 0 || tt.tt_insline == 0)
		goto no_scroll;
	if (dir > 0) {
		(*tt.tt_move)(row1x, 0);
		(*tt.tt_delline)();
		if (row2x < wwnrow) {
			(*tt.tt_move)(row2x - 1, 0);
			(*tt.tt_insline)();
		}
	} else {
		if (tt.tt_retain || row2x != wwnrow) {
			(*tt.tt_move)(row2x - 1, 0);
			(*tt.tt_delline)();
		}
		(*tt.tt_move)(row1x, 0);
		(*tt.tt_insline)();
	}
did_scroll:
	scrolled = 1;
	/*
	 * Fix up the old screen.
	 */
	{
		register union ww_char *tmp;
		register union ww_char **cpp, **cqq;

		if (dir > 0) {
			cpp = &wwos[row1x];
			cqq = cpp + 1;
			tmp = *cpp;
			for (i = row2x - row1x; --i > 0;)
				*cpp++ = *cqq++;
			*cpp = tmp;
		} else {
			cpp = &wwos[row2x];
			cqq = cpp - 1;
			tmp = *cqq;
			for (i = row2x - row1x; --i > 0;)
				*--cpp = *--cqq;
			*cqq = tmp;
		}
		for (i = wwncol; --i >= 0;)
			tmp++->c_w = ' ';
	}

no_scroll:
	/*
	 * Fix the new screen.
	 */
	if (nvis == nvismax) {
		/*
		 * Can shift whole lines.
		 */
		if (dir > 0) {
			{
				register union ww_char *tmp;
				register union ww_char **cpp, **cqq;

				cpp = &wwns[row1x];
				cqq = cpp + 1;
				tmp = *cpp;
				for (i = row2x - row1x; --i > 0;)
					*cpp++ = *cqq++;
				*cpp = tmp;
			}
			if (scrolled) {
				register char *p, *q;

				p = &wwtouched[row1x];
				q = p + 1;
				for (i = row2x - row1x; --i > 0;)
					*p++ = *q++;
				*p |= WWU_TOUCHED;
			} else {
				register char *p;

				p = &wwtouched[row1x];
				for (i = row2x - row1x; --i >= 0;)
					*p++ |= WWU_MAJOR|WWU_TOUCHED;
			}
			wwredrawwin1(w, row1, row1x, dir);
			wwredrawwin1(w, row2x - 1, row2 - leaveit, dir);
		} else {
			{
				register union ww_char *tmp;
				register union ww_char **cpp, **cqq;

				cpp = &wwns[row2x];
				cqq = cpp - 1;
				tmp = *cqq;
				for (i = row2x - row1x; --i > 0;)
					*--cpp = *--cqq;
				*cqq = tmp;
			}
			if (scrolled) {
				register char *p, *q;

				p = &wwtouched[row2x];
				q = p - 1;
				for (i = row2x - row1x; --i > 0;)
					*--p = *--q;
				*q |= WWU_TOUCHED;
			} else {
				register char *p;

				p = &wwtouched[row1x];
				for (i = row2x - row1x; --i >= 0;)
					*p++ |= WWU_MAJOR|WWU_TOUCHED;
			}
			wwredrawwin1(w, row1 + leaveit, row1x + 1, dir);
			wwredrawwin1(w, row2x, row2, dir);
		}
	} else {
		if (scrolled) {
			register char *p;

			p = &wwtouched[row1x];
			for (i = row2x - row1x; --i >= 0;)
				*p++ |= WWU_MAJOR|WWU_TOUCHED;
		}
out:
		if (dir > 0)
			wwredrawwin1(w, row1, row2 - leaveit, dir);
		else
			wwredrawwin1(w, row1 + leaveit, row2, dir);
	}
	return scrolled;
}

#ifndef lint
static char sccsid[] = "@(#)wwscroll.c	3.15 %G%";
#endif

/*
 * Copyright (c) 1983 Regents of the University of California,
 * All rights reserved.  Redistribution permitted subject to
 * the terms of the Berkeley Software License Agreement.
 */

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
	int deleted = 0;

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
	 * If it's a good idea to use delete and insert line
	 * and the terminal can, then do it.
	 */
	if (nvis > nvismax / 2 && tt.tt_delline && tt.tt_insline) {
		register union ww_char *tmp;
		register union ww_char **cpp, **cqq;

		/*
		 * Don't worry about retain when scrolling down,
		 * but do worry when scrolling up, for hp2621.
		 */
		if (dir > 0) {
			/*
			 * We're going to assume that a line feed at the
			 * bottom of the screen will cause a scroll, unless
			 * "ns" is set.  This should work at least 99%
			 * of the time.  At any rate, vi seems to do it.
			 */
			if (tt.tt_noscroll || row1x != 0 || row2x != wwnrow) {
				(*tt.tt_move)(row1x, 0);
				(*tt.tt_delline)();
				if (row2x < wwnrow) {
					(*tt.tt_move)(row2x - 1, 0);
					(*tt.tt_insline)();
				}
			} else {
				if (tt.tt_row != wwnrow - 1)
					(*tt.tt_move)(wwnrow - 1, 0);
				ttputc('\n');
			}
			/*
			 * Fix up the old screen.
			 */
			cpp = &wwos[row1x];
			cqq = cpp + 1;
			tmp = *cpp;
			for (i = row2x - row1x; --i > 0;)
				*cpp++ = *cqq++;
			*cpp = tmp;
		} else {
			if (tt.tt_retain || row2x != wwnrow) {
				(*tt.tt_move)(row2x - 1, 0);
				(*tt.tt_delline)();
			}
			(*tt.tt_move)(row1x, 0);
			(*tt.tt_insline)();
			/*
			 * Fix up the old screen.
			 */
			cpp = &wwos[row2x];
			cqq = cpp - 1;
			tmp = *cqq;
			for (i = row2x - row1x; --i > 0;)
				*--cpp = *--cqq;
			*cqq = tmp;
		}
		for (i = wwncol; --i >= 0;)
			tmp++->c_w = ' ';
		deleted++;
	}

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
			if (deleted) {
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
			if (deleted) {
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
		if (deleted) {
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
	return deleted;
}

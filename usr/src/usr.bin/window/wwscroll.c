#ifndef lint
static	char *sccsid = "@(#)wwscroll.c	3.9 83/09/14";
#endif

#include "ww.h"
#include "tt.h"

wwscroll(w, n)
register struct ww *w;
register n;
{
	int dir;
	register scroll;

	if (n == 0)
		return;
	dir = n < 0 ? -1 : 1;
	scroll = w->ww_scroll + n;
	if (scroll < 0)
		scroll = 0;
	else if (scroll > w->ww_nline - w->ww_w.nr)
		scroll = w->ww_nline - w->ww_w.nr;
	n = abs(scroll - w->ww_scroll);
	if (n < w->ww_i.nr) {
		while (--n >= 0) {
			(void) wwscroll1(w, w->ww_i.t - w->ww_w.t,
				w->ww_i.b - w->ww_w.t, dir, 0);
			w->ww_scroll += dir;
		}
	} else {
		w->ww_scroll = scroll;
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
		 * Don't worry about retain when scrolling down.
		 * But do worry when scrolling up.  For hp2621.
		 */
		if (dir > 0) {
			(*tt.tt_move)(row1x + w->ww_w.t, 0);
			(*tt.tt_delline)();
			if (row2x + w->ww_w.t < wwnrow) {
				(*tt.tt_move)(row2x + w->ww_w.t - 1, 0);
				(*tt.tt_insline)();
			}
			/*
			 * Fix up the old screen.
			 */
			cpp = &wwos[row1x + w->ww_w.t];
			cqq = cpp + 1;
			tmp = *cpp;
			for (i = row2x - row1x; --i > 0;)
				*cpp++ = *cqq++;
			*cpp = tmp;
			for (i = wwncol; --i >= 0;)
				tmp++->c_w = ' ';
		} else {
			if (tt.tt_retain || row2x + w->ww_w.t != wwnrow) {
				(*tt.tt_move)(row2x + w->ww_w.t - 1, 0);
				(*tt.tt_delline)();
			}
			(*tt.tt_move)(row1x + w->ww_w.t, 0);
			(*tt.tt_insline)();
			/*
			 * Fix up the old screen.
			 */
			cpp = &wwos[row2x + w->ww_w.t];
			cqq = cpp - 1;
			tmp = *cqq;
			for (i = row2x - row1x; --i > 0;)
				*--cpp = *--cqq;
			*cqq = tmp;
			for (i = wwncol; --i >= 0;)
				tmp++->c_w = ' ';
		}
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

				cpp = &wwns[row1x + w->ww_w.t];
				cqq = cpp + 1;
				tmp = *cpp;
				for (i = row2x - row1x; --i > 0;)
					*cpp++ = *cqq++;
				*cpp = tmp;
			}
			if (deleted) {
				register char *p, *q;

				p = &wwtouched[row1x + w->ww_w.t];
				q = p + 1;
				for (i = row2x - row1x; --i > 0;)
					*p++ = *q++;
				*p = 1;
			} else {
				register char *p;

				p = &wwtouched[row1x + w->ww_w.t];
				for (i = row2x - row1x; --i >= 0;)
					*p++ = 1;
			}
			wwredrawwin1(w, row1, row1x, w->ww_scroll + dir);
			wwredrawwin1(w, row2x - 1, row2 - leaveit,
				w->ww_scroll + dir);
		} else {
			{
				register union ww_char *tmp;
				register union ww_char **cpp, **cqq;

				cpp = &wwns[row2x + w->ww_w.t];
				cqq = cpp - 1;
				tmp = *cqq;
				for (i = row2x - row1x; --i > 0;)
					*--cpp = *--cqq;
				*cqq = tmp;
			}
			if (deleted) {
				register char *p, *q;

				p = &wwtouched[row2x + w->ww_w.t];
				q = p - 1;
				for (i = row2x - row1x; --i > 0;)
					*--p = *--q;
				*q = 1;
			} else {
				register char *p;

				p = &wwtouched[row1x + w->ww_w.t];
				for (i = row2x - row1x; --i >= 0;)
					*p++ = 1;
			}
			wwredrawwin1(w, row1 + leaveit, row1x - 1,
				w->ww_scroll + dir);
			wwredrawwin1(w, row2x, row2, w->ww_scroll + dir);
		}
	} else {
out:
		if (dir > 0)
			wwredrawwin1(w, row1, row2 - leaveit,
				w->ww_scroll + dir);
		else
			wwredrawwin1(w, row1 + leaveit, row2,
				w->ww_scroll + dir);
	}
	return deleted;
}

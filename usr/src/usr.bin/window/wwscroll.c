#ifndef lint
static	char *sccsid = "@(#)wwscroll.c	3.4 83/08/18";
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
	if (n < w->ww_w.nr) {
		while (--n >= 0) {
			wwscroll1(w, 0, w->ww_w.nr - 1, dir, 0);
			w->ww_scroll += dir;
		}
	} else {
		w->ww_scroll = scroll;
		wwredrawwin(w);
	}
}

/*
 * Scroll one line, between 'srow' and 'erow', in direction 'dir'.
 * Don't adjust ww_scroll.
 * And don't redraw 'leaveit' lines.
 */
wwscroll1(w, srow, erow, dir, leaveit)
register struct ww *w;
int srow, erow, dir;
int leaveit;
{
	register i;
	int srow1, erow1;
	int nvis;
	int nvismax;
	int deleted = 0;

	/*
	 * See how many lines on the screen are affected.
	 * And calculate srow, erow, and left at the same time.
	 */
	for (i = srow; i <= erow && w->ww_nvis[i] == 0; i++)
		;
	if ((srow1 = i) > erow) {
		/* can't do any fancy stuff */
		erow1 = srow1 - 1;
		goto out;
	}
	for (i = erow; i >= srow && w->ww_nvis[i] == 0; i--)
		;
	if ((erow1 = i) == srow1)
		goto out;		/* just one line is easy */

	/*
	 * See how much of this window is visible.
	 */
	nvismax = wwncol * (erow1 - srow1 + 1);
	nvis = 0;
	for (i = srow1; i <= erow1; i++)
		nvis += w->ww_nvis[i];

	/*
	 * If it's a good idea to use delete and insert line
	 * and the terminal can, then do it.
	 */
	if (nvis > nvismax / 2 && tt.tt_delline && tt.tt_insline) {
		register union ww_char *tmp;
		register union ww_char **cpp, **cqq;

		if (dir > 0) {
			(*tt.tt_move)(srow1 + w->ww_w.t, 0);
			(*tt.tt_delline)();
			if (erow1 + w->ww_w.t != wwnrow - 1) {
				(*tt.tt_move)(erow1 + w->ww_w.t, 0);
				(*tt.tt_insline)();
			}
			/*
			 * Fix up the old screen.
			 */
			cpp = &wwos[srow1 + w->ww_w.t];
			cqq = cpp + 1;
			tmp = *cpp;
			for (i = erow1 - srow1; --i >= 0;)
				*cpp++ = *cqq++;
			*cpp = tmp;
			for (i = wwncol; --i >= 0;)
				tmp++->c_w = ' ';
		} else {
			if (erow1 + w->ww_w.t != wwnrow - 1) {
				(*tt.tt_move)(erow1 + w->ww_w.t, 0);
				(*tt.tt_delline)();
			}
			(*tt.tt_move)(srow1 + w->ww_w.t, 0);
			(*tt.tt_insline)();
			/*
			 * Fix up the old screen.
			 */
			cqq = &wwos[erow1 + w->ww_w.t];
			cpp = cqq + 1;
			tmp = *cqq;
			for (i = erow1 - srow1; --i >= 0;)
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

				cpp = &wwns[srow1 + w->ww_w.t];
				cqq = cpp + 1;
				tmp = *cpp;
				for (i = erow1 - srow1; --i >= 0;)
					*cpp++ = *cqq++;
				*cpp = tmp;
			}
			{
				register char *p, *q;

				p = &wwtouched[srow1 + w->ww_w.t];
				q = p + 1;
				for (i = erow1 - srow1; --i >= 0;)
					*p++ = *q++;
				*p = 1;
			}
			wwredrawwin1(w, srow, srow1 - 1, w->ww_scroll + dir);
			wwredrawwin1(w, erow1, erow - leaveit,
				w->ww_scroll + dir);
		} else {
			{
				register union ww_char *tmp;
				register union ww_char **cpp, **cqq;

				cqq = &wwns[erow1 + w->ww_w.t];
				cpp = cqq + 1;
				tmp = *cqq;
				for (i = erow1 - srow1; --i >= 0;)
					*--cpp = *--cqq;
				*cqq = tmp;
			}
			{
				register char *p, *q;

				q = &wwtouched[erow1 + w->ww_w.t];
				p = q + 1;
				for (i = erow1 - srow1; --i >= 0;)
					*--p = *--q;
				*q = 1;
			}
			wwredrawwin1(w, srow + leaveit, srow1,
				w->ww_scroll + dir);
			wwredrawwin1(w, erow1 + 1, erow, w->ww_scroll + dir);
		}
	} else {
out:
		if (dir > 0)
			wwredrawwin1(w, srow, erow - leaveit,
				w->ww_scroll + dir);
		else
			wwredrawwin1(w, srow + leaveit, erow,
				w->ww_scroll + dir);
	}
	return deleted;
}

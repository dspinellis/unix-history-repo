#ifndef lint
static	char *sccsid = "@(#)wwscroll.c	1.2 83/08/12";
#endif

#include "ww.h"

/*
 * Scroll down one line, starting at 'line'.
 * Don't adjust ww_scroll.
 */
wwscroll1(w, srow, erow, dir, leaveit)
register struct ww *w;
int srow, erow, dir;
char leaveit;
{
	register i;
	int startrow, endrow;
	int nvis;
	int nvismax;
	int deleted = 0;

	/*
	 * See how many lines on the screen are affected.
	 * And calculate srow, erow, and left at the same time.
	 */
	for (i = srow; i <= erow && w->ww_nvis[i] == 0; i++)
		;
	if ((startrow = i) > erow) {
		/* can't do any fancy stuff */
		endrow = startrow - 1;
		goto out;
	}
	for (i = erow; i >= srow && w->ww_nvis[i] == 0; i--)
		;
	if ((endrow = i) == startrow)
		goto out;		/* just one line is easy */

	/*
	 * See how much of this window is visible.
	 */
	nvismax = wwncol * (endrow - startrow + 1);
	nvis = 0;
	for (i = startrow; i <= endrow; i++)
		nvis += w->ww_nvis[i];

	/*
	 * If it's a good idea to use delete and insert line
	 * and the terminal can, then do it.
	 */
	if (nvis > nvismax / 2 && tt.tt_delline && tt.tt_insline) {
		register union ww_char *tmp;
		register union ww_char **cpp, **cqq;

		if (dir > 0) {
			(*tt.tt_move)(startrow + w->ww_w.t, 0);
			(*tt.tt_delline)();
			if (endrow + w->ww_w.t != wwnrow - 1) {
				(*tt.tt_move)(endrow + w->ww_w.t, 0);
				(*tt.tt_insline)();
			}
			/*
			 * Fix up the old screen.
			 */
			cpp = &wwos[startrow + w->ww_w.t];
			cqq = cpp + 1;
			tmp = *cpp;
			for (i = endrow - startrow; --i >= 0;)
				*cpp++ = *cqq++;
			*cpp = tmp;
			for (i = wwncol; --i >= 0;)
				tmp++->c_w = ' ';
		} else {
			if (endrow + w->ww_w.t != wwnrow - 1) {
				(*tt.tt_move)(endrow + w->ww_w.t, 0);
				(*tt.tt_delline)();
			}
			(*tt.tt_move)(startrow + w->ww_w.t, 0);
			(*tt.tt_insline)();
			/*
			 * Fix up the old screen.
			 */
			cqq = &wwos[endrow + w->ww_w.t];
			cpp = cqq + 1;
			tmp = *cqq;
			for (i = endrow - startrow; --i >= 0;)
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
		register union ww_char *tmp;
		register union ww_char **cpp, **cqq;

		if (dir > 0) {
			cpp = &wwns[startrow + w->ww_w.t];
			cqq = cpp + 1;
			tmp = *cpp;
			for (i = endrow - startrow; --i >= 0;)
				*cpp++ = *cqq++;
			*cpp = tmp;
			wwredrawwin1(w, srow, startrow - 1, w->ww_scroll + dir);
			wwredrawwin1(w, endrow + 1, erow - leaveit,
				w->ww_scroll + dir);
		} else {
			cqq = &wwns[endrow + w->ww_w.t];
			cpp = cqq + 1;
			tmp = *cqq;
			for (i = endrow - startrow; --i >= 0;)
				*--cpp = *--cqq;
			*cqq = tmp;
			wwredrawwin1(w, srow + leaveit, startrow - 1,
				w->ww_scroll + dir);
			wwredrawwin1(w, endrow + 1, erow, w->ww_scroll + dir);
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

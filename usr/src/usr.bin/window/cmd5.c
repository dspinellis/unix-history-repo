#ifndef lint
static	char *sccsid = "@(#)cmd5.c	3.5 83/09/14";
#endif

#include "defs.h"

/*
 * Window movement.
 */
c_move(w)
register struct ww *w;
{
	int col, row;
	int mincol, minrow;
	int maxcol, maxrow;
	int curcol, currow;
	struct ww *back = w->ww_back;

	col = w->ww_w.l;
	row = w->ww_w.t;
	wwadd(boxwin, framewin->ww_back);
	for (;;) {
		wwbox(boxwin, row - 1, col - 1, w->ww_w.nr + 2, w->ww_w.nc + 2);
		getminmax(row, w->ww_w.nr, 1, wwnrow,
			&currow, &minrow, &maxrow);
		getminmax(col, w->ww_w.nc, 0, wwncol,
			&curcol, &mincol, &maxcol);
		wwsetcursor(currow, curcol);
		while (bpeekc() < 0)
			bread();
		wwunbox(boxwin);
		switch (getpos(&row, &col, minrow, mincol, maxrow, maxcol)) {
		case -1:
			wwdelete(boxwin);
			if (!terse)
				(void) wwputs("\r\nCancelled.  ", cmdwin);
			return;
		case 1:
			break;
		case 0:
			continue;
		}
		break;
	}
	wwdelete(boxwin);
	if (!terse)
		(void) wwputs("\r\n", cmdwin);
	wwcurtowin(cmdwin);
	wwdelete(w);
	wwmove(w, row, col);
	wwadd(w, back);
	reframe();
}

/*
 * Weird stufff, don't ask.
 */
getminmax(x, n, a, b, curx, minx, maxx)
register x, n, a, b;
int *curx, *minx, *maxx;
{
	if (x < a) {
		*curx = x + n - 1;
		*minx = 1 - n;
		*maxx = a;
	} else if (x == a) {
		*curx = x;
		*minx = 1 - n;
		*maxx = b - n;
	} else if (x < b - n) {
		*curx = x;
		*minx = a;
		*maxx = b - n;
	} else if (x == b - n) {
		*curx = x;
		*minx = a;
		*maxx = b - 1;
	} else {
		*curx = x;
		*minx = b - n;
		*maxx = b - 1;
	}
}

#ifndef lint
static	char *sccsid = "@(#)cmd5.c	3.7 83/09/15";
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
	movewin(w, row, col);
}

movewin(w, row, col)
register struct ww *w;
{
	struct ww *back = w->ww_back;

	w->ww_altpos.r = w->ww_w.t;
	w->ww_altpos.c = w->ww_w.l;
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
	if (x < 0)
		*curx = x + n - 1;
	else
		*curx = x;

	if (x <= a)
		*minx = 1 - n;
	else if (x <= b - n)
		*minx = a;
	else
		*minx = b - n;

	if (x >= b - n)
		*maxx = b - 1;
	else if (x >= a)
		*maxx = b - n;
	else
		*maxx = a;
}

#ifndef lint
static	char *sccsid = "@(#)cmd5.c	3.3 83/08/31";
#endif

#include "defs.h"

/*
 * Window movement.
 */
c_move(w)
register struct ww *w;
{
	int col, row;
	int tmp;
	struct ww *back = w->ww_back;

	col = w->ww_w.l;
	row = w->ww_w.t;
	wwadd(boxwin, framewin->ww_back);
	for (;;) {
		wwbox(boxwin, row - 1, col - 1, w->ww_w.nr + 2, w->ww_w.nc + 2);
		wwsetcursor(row, col);
		while (bpeekc() < 0)
			bread();
		wwunbox(boxwin);
		switch (getpos(&row, &col, 1, 0,
			wwnrow - w->ww_w.nr, wwncol - w->ww_w.nc)) {
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
	w->ww_w.t = row;
	w->ww_w.l = col;
	w->ww_w.b = row + w->ww_w.nr;
	w->ww_w.r = col + w->ww_w.nc;
	wwadd(w, back);
	reframe();
}

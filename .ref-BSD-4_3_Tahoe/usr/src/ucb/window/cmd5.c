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
static char sccsid[] = "@(#)cmd5.c	3.16 (Berkeley) 6/29/88";
#endif /* not lint */

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

	if (!terse)
		wwputs("New window position: ", cmdwin);
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
		while (wwpeekc() < 0)
			wwiomux();
		switch (getpos(&row, &col, minrow, mincol, maxrow, maxcol)) {
		case 3:
			wwunbox(boxwin);
			wwdelete(boxwin);
			return;
		case 2:
			wwunbox(boxwin);
			break;
		case 1:
			wwunbox(boxwin);
		case 0:
			continue;
		}
		break;
	}
	wwdelete(boxwin);
	if (!terse)
		wwputc('\n', cmdwin);
	wwcurtowin(cmdwin);
	movewin(w, row, col);
}

movewin(w, row, col)
register struct ww *w;
{
	struct ww *back = w->ww_back;

	w->ww_alt.t = w->ww_w.t;
	w->ww_alt.l = w->ww_w.l;
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

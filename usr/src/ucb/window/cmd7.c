#ifndef lint
static char sccsid[] = "@(#)cmd7.c	3.3 4/24/85";
#endif

/*
 * Copyright (c) 1983 Regents of the University of California,
 * All rights reserved.  Redistribution permitted subject to
 * the terms of the Berkeley Software License Agreement.
 */

#include "defs.h"

/*
 * Window size.
 */

c_size(w)
register struct ww *w;
{
	int col, row;

	if (!terse)
		wwputs("New window size (lower right corner): ", cmdwin);
	col = MIN(w->ww_w.r, wwncol) - 1;
	row = MIN(w->ww_w.b, wwnrow) - 1;
	wwadd(boxwin, framewin->ww_back);
	for (;;) {
		wwbox(boxwin, w->ww_w.t - 1, w->ww_w.l - 1,
			row - w->ww_w.t + 3, col - w->ww_w.l + 3);
		wwsetcursor(row, col);
		while (wwpeekc() < 0)
			wwiomux();
		switch (getpos(&row, &col, w->ww_w.t, w->ww_w.l,
			wwnrow - 1, wwncol - 1)) {
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
	sizewin(w, row - w->ww_w.t + 1, col - w->ww_w.l + 1);
}

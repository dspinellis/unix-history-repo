/*
 * Copyright (c) 1983 Regents of the University of California.
 * All rights reserved.
 *
 * This code is derived from software contributed to Berkeley by
 * Edward Wang at The University of California, Berkeley.
 *
 * Redistribution and use in source and binary forms are permitted provided
 * that: (1) source distributions retain this entire copyright notice and
 * comment, and (2) distributions including binaries display the following
 * acknowledgement:  ``This product includes software developed by the
 * University of California, Berkeley and its contributors'' in the
 * documentation or other materials provided with the distribution and in
 * all advertising materials mentioning features or use of this software.
 * Neither the name of the University nor the names of its contributors may
 * be used to endorse or promote products derived from this software without
 * specific prior written permission.
 * THIS SOFTWARE IS PROVIDED ``AS IS'' AND WITHOUT ANY EXPRESS OR IMPLIED
 * WARRANTIES, INCLUDING, WITHOUT LIMITATION, THE IMPLIED WARRANTIES OF
 * MERCHANTABILITY AND FITNESS FOR A PARTICULAR PURPOSE.
 */

#ifndef lint
static char sccsid[] = "@(#)cmd7.c	3.7 (Berkeley) 6/6/90";
#endif /* not lint */

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

/*
 * Copyright (c) 1983 Regents of the University of California.
 * All rights reserved.
 *
 * This code is derived from software contributed to Berkeley by
 * Edward Wang at The University of California, Berkeley.
 *
 * %sccs.include.redist.c%
 */

#ifndef lint
static char sccsid[] = "@(#)cmd1.c	3.36 (Berkeley) %G%";
#endif /* not lint */

#include "defs.h"
#include "char.h"

struct ww *doopen();

dowindow()
{
	int col, row, xcol, xrow;
	int id;

		return;
	if (!terse)
		wwputs("Upper left corner: ", cmdwin);
	col = 0;
	row = 1;
	wwadd(boxwin, framewin->ww_back);
	for (;;) {
		wwbox(boxwin, row - 1, col - 1, 3, 3);
		wwsetcursor(row, col);
		while (wwpeekc() < 0)
			wwiomux();
		switch (getpos(&row, &col, 0, 0)) {
		case 3:
			wwunbox(boxwin);
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
	if (!terse)
		wwputs("\r\nLower right corner: ", cmdwin);
	xcol = col + 1;
	xrow = row + 1;
	for (;;) {
		wwsetcursor(xrow, xcol);
		while (wwpeekc() < 0)
			wwiomux();
		switch (getpos(&xrow, &xcol, row + 1, col + 1)) {
		case 3:
			wwunbox(boxwin);
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
	if (!terse)
		wwputs("\r\n", cmdwin);
	wwsetcursor(WCurRow(cmdwin->ww_win), WCurCol(cmdwin->ww_win));
	if (doopen(id, xrow-row+1, xcol-col+1, row, col) == 0)
		if (terse)
			Ding();
		else
			wwputs("Can't open window.  ", cmdwin);
}

getpos(row, col, minrow, mincol, maxrow, maxcol)
register int *row, *col;
int minrow, mincol;
int maxrow, maxcol;
{
	static int scount;
	int count;
	char c;
	int oldrow = *row, oldcol = *col;

	while ((c = wwgetc()) >= 0) {
		switch (c) {
		case '0': case '1': case '2': case '3': case '4':
		case '5': case '6': case '7': case '8': case '9':
			scount = scount * 10 + c - '0';
			continue;
		}
		count = scount ? scount : 1;
		scount = 0;
		switch (c) {
		case 'h':
			if ((*col -= count) < mincol)
				*col = mincol;
			break;
		case 'H':
			*col = mincol;
			break;
		case 'l':
			if ((*col += count) > maxcol)
				*col = maxcol;
			break;
		case 'L':
			*col = maxcol;
			break;
		case 'j':
			if ((*row += count) > maxrow)
				*row = maxrow;
			break;
		case 'J':
			*row = maxrow;
			break;
		case 'k':
			if ((*row -= count) < minrow)
				*row = minrow;
			break;
		case 'K':
			*row = minrow;
			break;
		case ctrl('['):
			if (!terse)
				wwputs("\nCancelled.  ", cmdwin);
			return 3;
		case '\r':
			return 2;
		default:
			if (!terse)
				wwputs("\r\nType [hjklHJKL] to move, return to enter position, escape to cancel.", cmdwin);
			Ding();
		}
	}
	return oldrow != *row || oldcol != *col;
}

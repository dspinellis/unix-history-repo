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
static char sccsid[] = "@(#)wwframe.c	3.20 (Berkeley) %G%";
#endif /* not lint */

#include "ww.h"
#include "tt.h"

#define frameok(w, r, c) (w1 = wwindex[wwsmap[r][c]], \
	w1->ww_fmap || w1->ww_order > (w)->ww_order)

#define TOP	0
#define BOTTOM	1
#define LEFT	2
#define RIGHT	3
wwframe(w)
register struct ww *w;
{
	register r, c;
	char a1, a2, a3;
	char b1, b2, b3;
	register char *smap;
	register code;
	register struct ww *w1;
	char ulc, top, urc, left, right, llc, bottom, lrc;
	struct ww_dim oldsize;
	Pos bstart;

	oldsize = w->ww_w;
	w->ww_w = w->ww_i = w->ww_o;

	if (w->ww_o.col == 0)
		noleft = 1;
	else {
		noleft = 0;
		w->ww_i.ncol--;
		w->ww_i.col++;
	}
	/*
	if (w->ww_o.row == 0)
		notop++;
	else
	*/
	{
		notop = 0;
		w->ww_i.nrow--;
		w->ww_i.row++;
	}
	if (w->ww_o.col + w->ww_o.ncol == wwncol) {
		noright = 1;
	/*
	} else if (wwcheckframe(LEFT, w->ww_o.col + w->ww_o.ncol - 1,
			w->ww_o.row, w->ww_o.row + w->ww_o.nrow - 1, wwhead)) {
		noright = 1;
		w->ww_w.ncol--;
		w->ww_i.ncol--;
	} else if (wwcheckframe(LEFT, w->ww_o.col + w->ww_o.ncol,
			w->ww_o.row, w->ww_o.row + w->ww_o.nrow - 1, wwhead)) {
		XXXXX
		w->ww_w.ncol--;
	*/
	} else {
		noright = 0;
		w->ww_i.ncol--;
	}
	if (w->ww_o.row + w->ww_o.nrow == wwnrow) {
		nobot = 1;
	} else if (wwcheckframe(TOP, w->ww_o.row + w->ww_o.nrow - 1,
			w->ww_o.col, w->ww_o.col + w->ww_o.ncol - 1, wwhead)) {
		nobot = 1;
		w->ww_w.nrow--;
		w->ww_i.nrow--;
	/*
	} else if (wwcheckframe(TOP, w->ww_o.row + w->ww_o.nrow,
			w->ww_o.col, w->ww_o.col + w->ww_o.ncol - 1, wwhead)) {
		XXXXX
		ww->ww_i.nrow--;
	*/
	} else {
		nobot = 0;
		w->ww_i.nrow--;
	}

	if (oldsize.nrow != w->ww_w.nrow || oldsize.ncol != w->ww_w.ncol) {
		bstart = w->ww_win->w_bstart;
		if (Wsize(w->ww_win, w->ww_w.ncol, w->ww_w.nrow) != 0) {
			wwprintf(w, "wwframe: Wsize(%d, %d) failed.\r\n",
				w->ww_w.ncol, w->ww_w.nrow);
			return -1;
		}
		w->ww_win->w_bstart = bstart;
	}
	Wsetmargins(w->ww_win, noleft ? 0 : 1, notop ? 0 : 1,
		w->ww_i.ncol, w->ww_i.nrow);
	/* scroll to the old position */

	Wgetframe(&ulc, &top, &urc, &left, &right, &llc, &bottom, &lrc);

	}

	if (w->ww_w.b < wwnrow) {
		r = w->ww_w.b;
		c = w->ww_i.l - 1;
		smap = &wwsmap[r - 1][c + 1];
		a1 = 0;
		a2 = 0;
		b1 = 0;
		b2 = c < 0 || frameok(w, r, c);

	}

	}

	return 0;

wwcheckframe(flag, x, a, b, w)
register struct ww *w;

	if (a >= b)
		return 1;
	for (; w; w = w->ww_next) {
		switch (flag) {
		case TOP:
			xx = w->ww_o.row;
			aa = w->ww_o.col;
			bb = w->ww_o.col + w->ww_o.ncol - 1;
			break;
		case BOTTOM:
			xx = w->ww_o.row + w->ww_o.nrow - 1;
			aa = w->ww_o.col;
			bb = w->ww_o.col + w->ww_o.ncol - 1;
			break;
		case LEFT:
			xx = w->ww_o.col;
			aa = w->ww_o.row;
			bb = w->ww_o.row + w->ww_o.nrow - 1;
			break;
		case RIGHT:
			xx = w->ww_o.col + w->ww_o.ncol - 1;
			aa = w->ww_o.row;
			bb = w->ww_o.row + w->ww_o.nrow - 1;
			break;
		}
		if (xx != x || aa > b || bb < a)
			continue;
		return wwcheckframe(flag, x, a, aa, w->ww_next)
			&& wwcheckframe(flag, x, bb, b, w->ww_next);
	}
	return 0;
}

wwunframe(w)
	if (hastop) {
		Wauxcursor(w->ww_win, 0, 0);
		for (i = 0; i < w->ww_o.ncol; i++)
			Waputc(' ', WBUF, w->ww_win);
	}
	if (hasbot) {
		Wauxcursor(w->ww_win, w->ww_o.nrow - 1, 0);
		for (i = 0; i < w->ww_o.ncol; i++)
			Waputc(' ', WBUF, w->ww_win);
	}
	if (hasleft)
		for (i = 0; i < w->ww_o.nrow; i++) {
			Wauxcursor(w->ww_win, i, 0);
			Waputc(' ', WBUF, w->ww_win);
		}
	if (hasright)
		for (i = 0; i < w->ww_o.nrow; i++) {
			Wauxcursor(w->ww_win, i, w->ww_o.ncol - 1);
			Waputc(' ', WBUF, w->ww_win);
		}
	w->ww_i.row = w->ww_o.row;
	w->ww_i.nrow = w->ww_o.nrow;
	w->ww_i.col = w->ww_o.col;
	w->ww_i.ncol = w->ww_o.ncol;
	Wsetmargins(w->ww_win, 0, 0, w->ww_o.ncol, w->ww_o.nrow);
}

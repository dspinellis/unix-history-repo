#ifndef lint
static	char *sccsid = "@(#)wwframe.c	2.1 83/07/30";
#endif

#include "ww.h"

#define TOP	0
#define BOTTOM	1
#define LEFT	2
#define RIGHT	3

wwframe(w)
register struct ww *w;
{
	register i;
	char noleft, noright, notop, nobot;
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

	if (!notop) {
		Wauxcursor(w->ww_win, 0, 0);
		if (noleft)
			Waputc(top, 0, w->ww_win);
		else
			Waputc(ulc, 0, w->ww_win);
		for (i = w->ww_o.ncol - 2; i > 0; i--)
			Waputc(top, 0, w->ww_win);
		if (noright)
			Waputc(top, 0, w->ww_win);
		else
			Waputc(urc, 0, w->ww_win);
	}

	if (!nobot) {
		Wauxcursor(w->ww_win, w->ww_o.nrow - 1, 0);
		if (noleft)
			Waputc(bottom, 0, w->ww_win);
		else
			Waputc(llc, 0, w->ww_win);
		for (i = w->ww_o.ncol - 2; i > 0; i--)
			Waputc(bottom, 0, w->ww_win);
		if (noright)
			Waputc(bottom, 0, w->ww_win);
		else
			Waputc(lrc, 0, w->ww_win);
	}

	if (!noleft) {
		Wauxcursor(w->ww_win, 0, 0);
		if (notop)
			Waputc(left, 0, w->ww_win);
		else
			Waputc(ulc, 0, w->ww_win);
		for (i = 1; i < w->ww_o.nrow - 1; i++) {
			Wauxcursor(w->ww_win, i, 0);
			Waputc(left, 0, w->ww_win);
		}
		Wauxcursor(w->ww_win, w->ww_o.nrow - 1, 0);
		if (nobot)
			Waputc(left, 0, w->ww_win);
		else
			Waputc(llc, 0, w->ww_win);
	}

	if (!noright) {
		Wauxcursor(w->ww_win, 0, w->ww_o.ncol - 1);
		if (notop)
			Waputc(right, 0, w->ww_win);
		else
			Waputc(urc, 0, w->ww_win);
		for (i = 1; i < w->ww_o.nrow - 1; i++) {
			Wauxcursor(w->ww_win, i, w->ww_o.ncol - 1);
			Waputc(left, 0, w->ww_win);
		}
		Wauxcursor(w->ww_win, w->ww_o.nrow - 1, w->ww_o.ncol - 1);
		if (nobot)
			Waputc(right, 0, w->ww_win);
		else
			Waputc(lrc, 0, w->ww_win);
	}

	return 0;
}

wwcheckframe(flag, x, a, b, w)
register struct ww *w;
{
	int xx, aa, bb;

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
register struct ww *w;
{
	char hasbot, hastop, hasright, hasleft;
	register i;

	hastop = w->ww_o.row < w->ww_i.row;
	hasbot = w->ww_o.row + w->ww_o.nrow > w->ww_i.row + w->ww_i.nrow;
	hasleft = w->ww_o.col < w->ww_i.col;
	hasright = w->ww_o.col + w->ww_o.ncol > w->ww_i.col + w->ww_i.ncol;

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

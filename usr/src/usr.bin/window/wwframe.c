#ifndef lint
static	char *sccsid = "@(#)wwframe.c	1.2 83/07/19";
#endif

#include "ww.h"

wwframe(w)
register struct ww *w;
{
	register i;
	char noleft = 0;
	char noright = 0;
	char notop = 0;
	char nobot = 0;
	char ulc, top, urc, left, right, llc, bottom, lrc;

	w->ww_incol = w->ww_ncol;
	w->ww_inrow = w->ww_nrow;
	w->ww_icol = w->ww_col;
	w->ww_irow = w->ww_row;

	if (w->ww_col == 0)
		noleft++;
	/*
	if (w->ww_row == 0)
		notop++;
	*/
	if (w->ww_col + w->ww_ncol == WCols)
		noright++;
	if (w->ww_row + w->ww_nrow == WRows)
		nobot++;
	/*
	else if (wwcheckframe(w->ww_row + w->ww_nrow,
			w->ww_col, w->ww_col + w->ww_ncol - 1, wwhead))
		nobot++;
	else if (wwcheckframe(w->ww_row + w->ww_nrow - 1,
			w->ww_col, w->ww_col + w->ww_ncol - 1, wwhead)) {
		nobot++;
		ww
	}
	*/
	Wgetframe(&ulc, &top, &urc, &left, &right, &llc, &bottom, &lrc);

	if (!notop) {
		Wauxcursor(w->ww_win, 0, 0);
		if (noleft)
			Waputc(top, 0, w->ww_win);
		else
			Waputc(ulc, 0, w->ww_win);
		for (i = w->ww_ncol - 2; i > 0; i--)
			Waputc(top, 0, w->ww_win);
		if (noright)
			Waputc(top, 0, w->ww_win);
		else
			Waputc(urc, 0, w->ww_win);
		w->ww_irow++;
		w->ww_inrow--;
	}

	if (!nobot) {
		Wauxcursor(w->ww_win, w->ww_nrow - 1, 0);
		if (noleft)
			Waputc(bottom, 0, w->ww_win);
		else
			Waputc(llc, 0, w->ww_win);
		for (i = w->ww_ncol - 2; i > 0; i--)
			Waputc(bottom, 0, w->ww_win);
		if (noright)
			Waputc(bottom, 0, w->ww_win);
		else
			Waputc(lrc, 0, w->ww_win);
		w->ww_inrow--;
	}

	if (!noleft) {
		Wauxcursor(w->ww_win, 0, 0);
		if (notop)
			Waputc(left, 0, w->ww_win);
		else
			Waputc(ulc, 0, w->ww_win);
		for (i = 1; i < w->ww_nrow - 1; i++) {
			Wauxcursor(w->ww_win, i, 0);
			Waputc(left, 0, w->ww_win);
		}
		Wauxcursor(w->ww_win, w->ww_nrow - 1, 0);
		if (nobot)
			Waputc(left, 0, w->ww_win);
		else
			Waputc(llc, 0, w->ww_win);
		w->ww_icol++;
		w->ww_incol--;
	}

	if (!noright) {
		Wauxcursor(w->ww_win, 0, w->ww_ncol - 1);
		if (notop)
			Waputc(right, 0, w->ww_win);
		else
			Waputc(urc, 0, w->ww_win);
		for (i = 1; i < w->ww_nrow - 1; i++) {
			Wauxcursor(w->ww_win, i, w->ww_ncol - 1);
			Waputc(left, 0, w->ww_win);
		}
		Wauxcursor(w->ww_win, w->ww_nrow - 1, w->ww_ncol - 1);
		if (nobot)
			Waputc(right, 0, w->ww_win);
		else
			Waputc(lrc, 0, w->ww_win);
		w->ww_incol--;
	}

	Wsetmargins(w->ww_win, noleft ? 0 : 1, notop ? 0 : 1,
		w->ww_incol, w->ww_inrow);
}

wwunframe(w)
register struct ww *w;
{
	register i;

	w->ww_incol = w->ww_ncol;
	w->ww_inrow = w->ww_nrow;
	w->ww_icol = w->ww_col;
	w->ww_irow = w->ww_row;

	Wauxcursor(w->ww_win, 0, 0);
	for (i = w->ww_ncol; i > 0; i--)
		Waputc(' ', WBUF, w->ww_win);

	Wauxcursor(w->ww_win, w->ww_nrow - 1, 0);
	for (i = w->ww_ncol; i > 0; i--)
		Waputc(' ', WBUF, w->ww_win);

	for (i = 1; i < w->ww_nrow - 1; i++) {
		Wauxcursor(w->ww_win, i, 0);
		Waputc(' ', WBUF, w->ww_win);
		Wauxcursor(w->ww_win, i, w->ww_ncol - 1);
		Waputc(' ', WBUF, w->ww_win);
	}
	Wsetmargins(w->ww_win, 0, 0, w->ww_ncol, w->ww_nrow);
}

wwcheckbottom(w)
register struct ww *w;
{
	return wwcheckframe(w->ww_row + w->ww_nrow,
		w->ww_col, w->ww_col + w->ww_ncol - 1, wwhead);
}

wwcheckframe(row, l, r, w)
register struct ww *w;
{
	int rr;

	if (l >= r)
		return 1;
	for (; w; w = w->ww_next) {
		if (w->ww_row != row)
			continue;
		if (w->ww_col > r)
			continue;
		if ((rr = w->ww_col + w->ww_ncol) <= l)
			continue;
		return wwcheckframe(row, l, w->ww_col - 1, w->ww_next)
			&& wwcheckframe(row, rr, r, w->ww_next);
	}
	return 0;
}

#ifndef lint
static char sccsid[] = "@(#)wwdelete.c	3.14 %G%";
#endif

/*
 * Copyright (c) 1983 Regents of the University of California,
 * All rights reserved.  Redistribution permitted subject to
 * the terms of the Berkeley Software License Agreement.
 */

#include "ww.h"

/*
 * Pull w free from the cover list.
 */
wwdelete(w)
register struct ww *w;
{
	register i;

	for (i = w->ww_i.t; i < w->ww_i.b; i++) {
		register j;
		register char *smap = wwsmap[i];
		register union ww_char *ns = wwns[i];
		register int nchanged = 0;

		for (j = w->ww_i.l; j < w->ww_i.r; j++)
			if (smap[j] == w->ww_index) {
				smap[j] = WWX_NOBODY;
				ns[j].c_w = ' ';
				nchanged++;
			}
		if (nchanged > 4)
			wwtouched[i] |= WWU_MAJOR|WWU_TOUCHED;
		else if (nchanged > 0)
			wwtouched[i] |= WWU_TOUCHED;
	}

	{
		register struct ww *wp;

		for (wp = w->ww_forw; wp != &wwhead; wp = wp->ww_forw)
			wp->ww_order--;
	}

	if (w->ww_forw != &wwhead)
		wwdelete1(w->ww_forw,
			w->ww_i.t, w->ww_i.b, w->ww_i.l, w->ww_i.r);

	w->ww_back->ww_forw = w->ww_forw;
	w->ww_forw->ww_back = w->ww_back;
	w->ww_forw = w->ww_back = 0;
}

wwdelete1(w, t, b, l, r)
register struct ww *w;
{
	int i;
	int tt, bb, ll, rr;
	char hasglass;

again:
	hasglass = 0;
	tt = MAX(t, w->ww_i.t);
	bb = MIN(b, w->ww_i.b);
	ll = MAX(l, w->ww_i.l);
	rr = MIN(r, w->ww_i.r);
	if (tt >= bb || ll >= rr) {
		if ((w = w->ww_forw) == &wwhead)
			return;
		goto again;
	}
	for (i = tt; i < bb; i++) {
		register j;
		register char *smap = wwsmap[i];
		register union ww_char *ns = wwns[i];
		register char *win = w->ww_win[i];
		register union ww_char *buf = w->ww_buf[i];
		int nvis = w->ww_nvis[i];
		int nchanged = 0;

		for (j = ll; j < rr; j++) {
			if (smap[j] != WWX_NOBODY)
				continue;
			if (win[j] & WWM_GLS) {
				hasglass = 1;
				continue;
			}
			smap[j] = w->ww_index;
			ns[j].c_w = buf[j].c_w ^ win[j] << WWC_MSHIFT;
			nchanged++;
			if (win[j] == 0)
				nvis++;
		}
		if (nchanged > 4)
			wwtouched[i] |= WWU_MAJOR|WWU_TOUCHED;
		else if (nchanged > 0)
			wwtouched[i] |= WWU_TOUCHED;
		w->ww_nvis[i] = nvis;
	}
	if ((w = w->ww_forw) == &wwhead)
		return;
	if (hasglass)
		goto again;
	if (tt > t)
		wwdelete1(w, t, tt, l, r);
	if (bb < b)
		wwdelete1(w, bb, b, l, r);
	if (ll > l)
		wwdelete1(w, tt, bb, l, ll);
	if (rr < r)
		wwdelete1(w, tt, bb, rr, r);
}

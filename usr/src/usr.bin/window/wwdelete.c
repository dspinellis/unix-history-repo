#ifndef lint
static	char *sccsid = "@(#)wwdelete.c	3.8 83/11/28";
#endif

#include "ww.h"

/*
 * Pull w free from the cover list.
 */
wwdelete(w)
register struct ww *w;
{
	register struct ww *wp;
	register i, j;

	for (i = w->ww_i.t; i < w->ww_i.b; i++)
		for (j = w->ww_i.l; j < w->ww_i.r; j++)
			if (wwsmap[i][j] == w->ww_index) {
				wwsmap[i][j] = WWX_NOBODY;
				wwns[i][j].c_w = ' ';
				wwtouched[i] = 1;
			}

	for (wp = w->ww_forw; wp != &wwhead; wp = wp->ww_forw)
		w->ww_order--;

	if ((wp = w->ww_forw) != &wwhead)
		wwdelete1(wp, w->ww_i.t, w->ww_i.b, w->ww_i.l, w->ww_i.r);

	w->ww_back->ww_forw = w->ww_forw;
	w->ww_forw->ww_back = w->ww_back;
	w->ww_forw = w->ww_back = 0;
}

wwdelete1(w, t, b, l, r)
register struct ww *w;
{
	int i;
	int tt, bb, ll, rr;

	tt = MAX(t, w->ww_i.t);
	bb = MIN(b, w->ww_i.b);
	ll = MAX(l, w->ww_i.l);
	rr = MIN(r, w->ww_i.r);
	for (i = tt; i < bb; i++) {
		register j = ll;
		register char *smap = wwsmap[i];
		register union ww_char *ns = wwns[i];
		register char *win = w->ww_win[i];
		register union ww_char *buf = w->ww_buf[i];
		int nvis = w->ww_nvis[i];
		char touched = wwtouched[i];

		for (j = ll; j < rr; j++) {
			if (smap[j] != WWX_NOBODY)
				continue;
			if ((win[j] & WWM_GLS) == 0) {
				smap[j] = w->ww_index;
				ns[j].c_w = buf[j].c_w ^ win[j] << WWC_MSHIFT;
				touched = 1;
				if (win[j] == 0)
					nvis++;
			}
		}
		wwtouched[i] = touched;
		w->ww_nvis[i] = nvis;
	}
	if ((w = w->ww_forw) == &wwhead)
		return;
	if (tt > t)
		wwdelete1(w, t, tt, l, r);
	if (bb < b)
		wwdelete1(w, bb, b, l, r);
	if (ll > l)
		wwdelete1(w, t, b, l, ll);
	if (rr < r)
		wwdelete1(w, t, b, rr, r);
}

#ifndef lint
static	char *sccsid = "@(#)wwframe.c	3.1 83/08/11";
#endif

#include "ww.h"

char **wwfmap;
#define U 1
#define R 2
#define D 4
#define L 8

wwframe(w, wframe)
register struct ww *w, *wframe;
{
	register i;
	char noleft, noright, notop, nobot;

	if (wwfmap == 0
	    && (wwfmap = wwalloc(wwnrow, wwncol, sizeof (char))) == 0)
		return -1;
	noleft = w->ww_w.l == 0;
	noright = w->ww_w.r >= wwncol;
	notop = w->ww_w.t == 0;
	nobot = w->ww_w.b >= wwnrow;

	if (!notop) {
		for (i = w->ww_w.l; i < w->ww_w.r; i++)
			wwframex(w, w->ww_w.t, i, wframe);
	}

	if (!nobot) {
		for (i = w->ww_w.l; i < w->ww_w.r; i++)
			wwframex(w, w->ww_w.b - 1, i, wframe);
	}

	if (!noleft) {
		for (i = w->ww_w.t; i < w->ww_w.b; i++)
			wwframex(w, i, w->ww_w.l, wframe);
	}

	if (!noright) {
		for (i = w->ww_w.t; i < w->ww_w.b; i++)
			wwframex(w, i, w->ww_w.r - 1, wframe);
	}
}

wwframex(w, r, c, wframe)
register struct ww *w, *wframe;
register r, c;
{
	char ul, top, ur, right, lr, bottom, ll, left;

	if (w->ww_index != wwsmap[r][c])
		return;
	ul = wwframeok(w, r - 1, c - 1);
	top = wwframeok(w, r - 1, c);
	ur = wwframeok(w, r - 1, c + 1);
	right = wwframeok(w, r, c + 1);
	lr = wwframeok(w, r + 1, c + 1);
	bottom = wwframeok(w, r + 1, c);
	ll = wwframeok(w, r + 1, c - 1);
	left = wwframeok(w, r, c - 1);
	if (top && ul) {
		wwframec(r - 1, c - 1, wframe, R);
		wwframec(r - 1, c, wframe, L);
	}
	if (top && ur) {
		wwframec(r - 1, c, wframe, R);
		wwframec(r - 1, c + 1, wframe, L);
	}
	if (right && ur) {
		wwframec(r - 1, c + 1, wframe, D);
		wwframec(r, c + 1, wframe, U);
	}
	if (right && lr) {
		wwframec(r, c + 1, wframe, D);
		wwframec(r + 1, c + 1, wframe, U);
	}
	if (bottom && lr) {
		wwframec(r + 1, c + 1, wframe, L);
		wwframec(r + 1, c, wframe, R);
	}
	if (bottom && ll) {
		wwframec(r + 1, c, wframe, L);
		wwframec(r + 1, c - 1, wframe, R);
	}
	if (left && ll) {
		wwframec(r + 1, c - 1, wframe, U);
		wwframec(r, c - 1, wframe, D);
	}
	if (left && ul) {
		wwframec(r, c - 1, wframe, U);
		wwframec(r - 1, c - 1, wframe, D);
	}
}

wwframeok(w, r, c)
register struct ww *w;
{
	register struct ww *w1;

	if (r < 0 || r >= wwnrow || c < 0 || c >= wwncol)
		return 1;
	w1 = wwindex[wwsmap[r][c]];
	if (w1->ww_hasframe && w1->ww_order <= w->ww_order)
		return 0;
	return 1;
}

wwframec(rr, cc, f, code)
register struct ww *f;
register rr, cc;
{
	register struct ww *w2;
	register r, c;

	if (rr < 0 || rr >= wwnrow || cc < 0 || cc >= wwncol)
		return;
	w2 = wwindex[wwsmap[rr][cc]];
	if (w2->ww_order > f->ww_order) {
		if (w2 != &wwnobody) {
			r = rr - w2->ww_w.t;
			c = cc - w2->ww_w.l;
			if ((w2->ww_win[r][c] |= WWM_COV) == WWM_COV)
				w2->ww_nvis[r]--;
			w2->ww_cov[r][c] = f->ww_index;
		}
		wwsmap[rr][cc] = f->ww_index;
	}
	code = wwfmap[rr][cc] |= code;
	r = rr - f->ww_w.t;
	c = cc - f->ww_w.l;
	if (f->ww_win[r][c] == WWM_GLS)
		f->ww_nvis[r]++;
	f->ww_win[r][c] &= ~WWM_GLS;
	f->ww_buf[f->ww_scroll + r][c].c_w = tt.tt_frame[code] & WWC_CMASK;
	if (wwsmap[rr][cc] == f->ww_index)
		wwns[rr][cc].c_w = tt.tt_frame[code] & WWC_CMASK;
}

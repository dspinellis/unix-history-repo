#ifndef lint
static	char *sccsid = "@(#)wwframe.c	3.2 83/08/12";
#endif

#include "ww.h"

wwframe(w, wframe)
register struct ww *w;
struct ww *wframe;
{
	register r, c;
	char a1, a2, a3;
	char b1, b2, b3;
	register char *smap;
	register code;

	if (w->ww_w.t > 0) {
		r = w->ww_w.t - 1;
		c = w->ww_w.l - 1;
		smap = &wwsmap[r + 1][c + 1];
		a1 = 0;
		a2 = 0;
		b1 = 0;
		b2 = wwframeok(w, r, c);

		for (; c < w->ww_w.r; c++) {
			a3 = w->ww_index == *smap++;
			b3 = wwframeok(w, r, c + 1);
			if (b2) {
				code = 0;
				if ((a1 || a2) && b1)
					code |= WWF_L;
				if ((a2 || a3) && b3)
					code |= WWF_R;
				if (code)
					wwframec(r, c, wframe, code|WWF_TOP);
			}
			a1 = a2;
			a2 = a3;
			b1 = b2;
			b2 = b3;
		}
		if ((a1 || a2) && b1 && b2)
			wwframec(r, c, wframe, WWF_L|WWF_TOP);
	}

	if (w->ww_w.b < wwnrow) {
		r = w->ww_w.b;
		c = w->ww_w.l - 1;
		smap = &wwsmap[r - 1][c + 1];
		a1 = 0;
		a2 = 0;
		b1 = 0;
		b2 = wwframeok(w, r, c);

		for (; c < w->ww_w.r; c++) {
			a3 = w->ww_index == *smap++;
			b3 = wwframeok(w, r, c + 1);
			if (b2) {
				code = 0;
				if ((a1 || a2) && b1)
					code |= WWF_L;
				if ((a2 || a3) && b3)
					code |= WWF_R;
				if (code)
					wwframec(r, c, wframe, code);
			}
			a1 = a2;
			a2 = a3;
			b1 = b2;
			b2 = b3;
		}
		if ((a1 || a2) && b1 && b2)
			wwframec(r, c, wframe, WWF_L);
	}

	if (w->ww_w.l > 0) {
		r = w->ww_w.t - 1;
		c = w->ww_w.l - 1;
		a1 = 0;
		a2 = 0;
		b1 = 0;
		b2 = wwframeok(w, r, c);

		for (; r < w->ww_w.b; r++) {
			a3 = w->ww_index == wwsmap[r + 1][c + 1];
			b3 = wwframeok(w, r + 1, c);
			if (b2) {
				code = 0;
				if ((a1 || a2) && b1)
					code |= WWF_U;
				if ((a2 || a3) && b3)
					code |= WWF_D;
				if (code)
					wwframec(r, c, wframe, code);
			}
			a1 = a2;
			a2 = a3;
			b1 = b2;
			b2 = b3;
		}
		if ((a1 || a2) && b1 && b2)
			wwframec(r, c, wframe, WWF_U);
	}

	if (w->ww_w.r < wwncol) {
		r = w->ww_w.t - 1;
		c = w->ww_w.r;
		a1 = 0;
		a2 = 0;
		b1 = 0;
		b2 = wwframeok(w, r, c);

		for (; r < w->ww_w.b; r++) {
			a3 = w->ww_index == wwsmap[r + 1][c - 1];
			b3 = wwframeok(w, r + 1, c);
			if (b2) {
				code = 0;
				if ((a1 || a2) && b1)
					code |= WWF_U;
				if ((a2 || a3) && b3)
					code |= WWF_D;
				if (code)
					wwframec(r, c, wframe, code);
			}
			a1 = a2;
			a2 = a3;
			b1 = b2;
			b2 = b3;
		}
		if ((a1 || a2) && b1 && b2)
			wwframec(r, c, wframe, WWF_U);
	}
}

wwframeok(w, r, c)
struct ww *w;
register r, c;
{
	register struct ww *w1;

	if (r < 0 || r >= wwnrow || c < 0 || c >= wwncol)
		return 1;
	w1 = wwindex[wwsmap[r][c]];
	return !w1->ww_hasframe || w1->ww_order > w->ww_order;
}

wwframec(rr, cc, f, code)
register struct ww *f;
register rr, cc;
int code;
{
	register r, c;

	if (rr < 0 || rr >= wwnrow || cc < 0 || cc >= wwncol)
		return;
	{
		register struct ww *w;
		w = wwindex[wwsmap[rr][cc]];
		if (w->ww_order > f->ww_order) {
			if (w != &wwnobody) {
				r = rr - w->ww_w.t;
				c = cc - w->ww_w.l;
				if ((w->ww_win[r][c] |= WWM_COV) == WWM_COV)
					w->ww_nvis[r]--;
				w->ww_cov[r][c] = f->ww_index;
			}
			wwsmap[rr][cc] = f->ww_index;
		}
	}
	{
		register char *fmap;
		fmap = &wwfmap[rr][cc];
		*fmap |= code;
		if (code & WWF_TOP)
			*fmap &= ~WWF_LABEL;
		code = *(unsigned char *)fmap;
	}
	r = rr - f->ww_w.t;
	c = cc - f->ww_w.l;
	if (f->ww_win[r][c] == WWM_GLS)
		f->ww_nvis[r]++;
	f->ww_win[r][c] &= ~WWM_GLS;
	if ((code & WWF_LABEL) == 0) {
		register tmp;

		tmp = tt.tt_frame[code & WWF_MASK] & WWC_CMASK;
		f->ww_buf[f->ww_scroll + r][c].c_w = tmp;
		if (wwsmap[rr][cc] == f->ww_index)
			wwns[rr][cc].c_w = tmp;
	}
}

/*
wwckns()
{
	register i, j;

	for (i = 0; i < wwnrow; i++)
		for (j = 0; j < wwncol; j++)
			if ((wwns[i][j].c_c & 0x7f) < ' ')
				abort();
}
*/

#ifndef lint
static	char *sccsid = "@(#)wwframe.c	3.5 83/08/18";
#endif

#include "ww.h"
#include "tt.h"

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
					wwframec(wframe, r, c, code|WWF_TOP, 1);
			}
			a1 = a2;
			a2 = a3;
			b1 = b2;
			b2 = b3;
		}
		if ((a1 || a2) && b1 && b2)
			wwframec(wframe, r, c, WWF_L|WWF_TOP, 1);
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
					wwframec(wframe, r, c, code, 1);
			}
			a1 = a2;
			a2 = a3;
			b1 = b2;
			b2 = b3;
		}
		if ((a1 || a2) && b1 && b2)
			wwframec(wframe, r, c, WWF_L, 1);
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
					wwframec(wframe, r, c, code, 1);
			}
			a1 = a2;
			a2 = a3;
			b1 = b2;
			b2 = b3;
		}
		if ((a1 || a2) && b1 && b2)
			wwframec(wframe, r, c, WWF_U, 1);
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
					wwframec(wframe, r, c, code, 1);
			}
			a1 = a2;
			a2 = a3;
			b1 = b2;
			b2 = b3;
		}
		if ((a1 || a2) && b1 && b2)
			wwframec(wframe, r, c, WWF_U, 1);
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

wwframec(f, rr, cc, code, dofmap)
register struct ww *f;
register rr, cc;
int code;
char dofmap;
{
	register r, c;

	if (rr < f->ww_w.t || rr >= f->ww_w.b
	    || cc < f->ww_w.l || cc >= f->ww_w.r)
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
	if (dofmap) {
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
		if (wwsmap[rr][cc] == f->ww_index) {
			wwtouched[rr] = 1;
			wwns[rr][cc].c_w = tmp;
		}
	}
}

#ifndef lint
static	char *sccsid = "@(#)wwframe.c	3.6 83/08/19";
#endif

#include "ww.h"
#include "tt.h"

#define frameok(w, r, c) (w1 = wwindex[wwsmap[r][c]], \
	!w1->ww_hasframe || w1->ww_order > (w)->ww_order)

wwframe(w, wframe)
register struct ww *w;
struct ww *wframe;
{
	register r, c;
	char a1, a2, a3;
	char b1, b2, b3;
	register char *smap;
	register code;
	register struct ww *w1;

	if (w->ww_w.t > 0) {
		r = w->ww_w.t - 1;
		c = w->ww_w.l - 1;
		smap = &wwsmap[r + 1][c + 1];
		a1 = 0;
		a2 = 0;
		b1 = 0;
		b2 = c < 0 || frameok(w, r, c);

		for (; c < w->ww_w.r; c++) {
			a3 = w->ww_index == *smap++;
			b3 = c + 1 >= wwncol || frameok(w, r, c + 1);
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
		b2 = c < 0 || frameok(w, r, c);

		for (; c < w->ww_w.r; c++) {
			a3 = w->ww_index == *smap++;
			b3 = c + 1 >= wwncol || frameok(w, r, c + 1);
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
		b2 = r < 0 || frameok(w, r, c);

		for (; r < w->ww_w.b; r++) {
			a3 = w->ww_index == wwsmap[r + 1][c + 1];
			b3 = r + 1 >= wwnrow || frameok(w, r + 1, c);
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
		b2 = r < 0 || frameok(w, r, c);

		for (; r < w->ww_w.b; r++) {
			a3 = w->ww_index == wwsmap[r + 1][c - 1];
			b3 = r + 1 >= wwnrow || frameok(w, r + 1, c);
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

wwframec(f, rr, cc, code, dofmap)
register struct ww *f;
register rr, cc;
char code;
char dofmap;
{
	char oldcode;

	if (rr < f->ww_w.t || rr >= f->ww_w.b
	    || cc < f->ww_w.l || cc >= f->ww_w.r)
		return;
	{
		register struct ww *w;
		register r, c;

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
		oldcode = *fmap;
		*fmap |= code;
		if (code & WWF_TOP)
			*fmap &= ~WWF_LABEL;
		code = *fmap;
	} else
		oldcode = 0;
	{
		register r, c;

		r = rr - f->ww_w.t;
		c = cc - f->ww_w.l;
		{
			register char *win = &f->ww_win[r][c];

			if (*win == WWM_GLS)
				f->ww_nvis[r]++;
			*win &= ~WWM_GLS;
		}
		if (oldcode != code && (code & WWF_LABEL) == 0) {
			register short frame;

			frame = tt.tt_frame[code & WWF_MASK] & WWC_CMASK;
			f->ww_buf[f->ww_scroll + r][c].c_w = frame;
			if (wwsmap[rr][cc] == f->ww_index) {
				wwtouched[rr] = 1;
				wwns[rr][cc].c_w = frame;
			}
		}
	}
}

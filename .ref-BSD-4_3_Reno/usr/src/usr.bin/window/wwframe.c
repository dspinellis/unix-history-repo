/*
 * Copyright (c) 1983 Regents of the University of California.
 * All rights reserved.
 *
 * This code is derived from software contributed to Berkeley by
 * Edward Wang at The University of California, Berkeley.
 *
 * Redistribution and use in source and binary forms are permitted provided
 * that: (1) source distributions retain this entire copyright notice and
 * comment, and (2) distributions including binaries display the following
 * acknowledgement:  ``This product includes software developed by the
 * University of California, Berkeley and its contributors'' in the
 * documentation or other materials provided with the distribution and in
 * all advertising materials mentioning features or use of this software.
 * Neither the name of the University nor the names of its contributors may
 * be used to endorse or promote products derived from this software without
 * specific prior written permission.
 * THIS SOFTWARE IS PROVIDED ``AS IS'' AND WITHOUT ANY EXPRESS OR IMPLIED
 * WARRANTIES, INCLUDING, WITHOUT LIMITATION, THE IMPLIED WARRANTIES OF
 * MERCHANTABILITY AND FITNESS FOR A PARTICULAR PURPOSE.
 */

#ifndef lint
static char sccsid[] = "@(#)wwframe.c	3.20 (Berkeley) 6/6/90";
#endif /* not lint */

#include "ww.h"
#include "tt.h"

#define frameok(w, r, c) (w1 = wwindex[wwsmap[r][c]], \
	w1->ww_fmap || w1->ww_order > (w)->ww_order)

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
		c = w->ww_i.l - 1;
		smap = &wwsmap[r + 1][c + 1];
		a1 = 0;
		a2 = 0;
		b1 = 0;
		b2 = c < 0 || frameok(w, r, c);

		for (; c < w->ww_i.r; c++) {
			if (c + 1 >= wwncol) {
				a3 = 1;
				b3 = 1;
			} else {
				a3 = w->ww_index == *smap++;
				b3 = frameok(w, r, c + 1);
			}
			if (b2) {
				code = 0;
				if ((a1 || a2) && b1)
					code |= WWF_L;
				if ((a2 || a3) && b3)
					code |= WWF_R;
				if (code)
					wwframec(wframe, r, c, code|WWF_TOP);
			}
			a1 = a2;
			a2 = a3;
			b1 = b2;
			b2 = b3;
		}
		if ((a1 || a2) && b1 && b2)
			wwframec(wframe, r, c, WWF_L|WWF_TOP);
	}

	if (w->ww_w.b < wwnrow) {
		r = w->ww_w.b;
		c = w->ww_i.l - 1;
		smap = &wwsmap[r - 1][c + 1];
		a1 = 0;
		a2 = 0;
		b1 = 0;
		b2 = c < 0 || frameok(w, r, c);

		for (; c < w->ww_i.r; c++) {
			if (c + 1 >= wwncol) {
				a3 = 1;
				b3 = 1;
			} else {
				a3 = w->ww_index == *smap++;
				b3 = frameok(w, r, c + 1);
			}
			if (b2) {
				code = 0;
				if ((a1 || a2) && b1)
					code |= WWF_L;
				if ((a2 || a3) && b3)
					code |= WWF_R;
				if (code)
					wwframec(wframe, r, c, code);
			}
			a1 = a2;
			a2 = a3;
			b1 = b2;
			b2 = b3;
		}
		if ((a1 || a2) && b1 && b2)
			wwframec(wframe, r, c, WWF_L);
	}

	if (w->ww_w.l > 0) {
		r = w->ww_i.t - 1;
		c = w->ww_w.l - 1;
		a1 = 0;
		a2 = 0;
		b1 = 0;
		b2 = r < 0 || frameok(w, r, c);

		for (; r < w->ww_i.b; r++) {
			if (r + 1 >= wwnrow) {
				a3 = 1;
				b3 = 1;
			} else {
				a3 = w->ww_index == wwsmap[r + 1][c + 1];
				b3 = frameok(w, r + 1, c);
			}
			if (b2) {
				code = 0;
				if ((a1 || a2) && b1)
					code |= WWF_U;
				if ((a2 || a3) && b3)
					code |= WWF_D;
				if (code)
					wwframec(wframe, r, c, code);
			}
			a1 = a2;
			a2 = a3;
			b1 = b2;
			b2 = b3;
		}
		if ((a1 || a2) && b1 && b2)
			wwframec(wframe, r, c, WWF_U);
	}

	if (w->ww_w.r < wwncol) {
		r = w->ww_i.t - 1;
		c = w->ww_w.r;
		a1 = 0;
		a2 = 0;
		b1 = 0;
		b2 = r < 0 || frameok(w, r, c);

		for (; r < w->ww_i.b; r++) {
			if (r + 1 >= wwnrow) {
				a3 = 1;
				b3 = 1;
			} else {
				a3 = w->ww_index == wwsmap[r + 1][c - 1];
				b3 = frameok(w, r + 1, c);
			}
			if (b2) {
				code = 0;
				if ((a1 || a2) && b1)
					code |= WWF_U;
				if ((a2 || a3) && b3)
					code |= WWF_D;
				if (code)
					wwframec(wframe, r, c, code);
			}
			a1 = a2;
			a2 = a3;
			b1 = b2;
			b2 = b3;
		}
		if ((a1 || a2) && b1 && b2)
			wwframec(wframe, r, c, WWF_U);
	}
}

wwframec(f, r, c, code)
register struct ww *f;
register r, c;
char code;
{
	char oldcode;
	register char *smap;

	if (r < f->ww_i.t || r >= f->ww_i.b || c < f->ww_i.l || c >= f->ww_i.r)
		return;

	smap = &wwsmap[r][c];

	{
		register struct ww *w;

		w = wwindex[*smap];
		if (w->ww_order > f->ww_order) {
			if (w != &wwnobody && w->ww_win[r][c] == 0)
				w->ww_nvis[r]--;
			*smap = f->ww_index;
		}
	}

	if (f->ww_fmap != 0) {
		register char *fmap;

		fmap = &f->ww_fmap[r][c];
		oldcode = *fmap;
		*fmap |= code;
		if (code & WWF_TOP)
			*fmap &= ~WWF_LABEL;
		code = *fmap;
	} else
		oldcode = 0;
	{
		register char *win = &f->ww_win[r][c];

		if (*win == WWM_GLS && *smap == f->ww_index)
			f->ww_nvis[r]++;
		*win &= ~WWM_GLS;
	}
	if (oldcode != code && (code & WWF_LABEL) == 0) {
		register short frame;

		frame = tt.tt_frame[code & WWF_MASK];
		f->ww_buf[r][c].c_w = frame;
		if (wwsmap[r][c] == f->ww_index) {
			wwtouched[r] |= WWU_TOUCHED;
			wwns[r][c].c_w = frame;
		}
	}
}

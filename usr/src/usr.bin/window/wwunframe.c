#ifndef lint
static	char *sccsid = "@(#)wwunframe.c	3.9 83/11/23";
#endif

#include "ww.h"

wwunframe(w)
register struct ww *w;
{
	register i, j;
	register char *win;
	register char *fmap;
	register struct ww *wp;

	for (i = w->ww_i.t; i < w->ww_i.b; i++) {
		j = w->ww_i.l;
		win = &w->ww_win[i][j];
		if (w->ww_fmap)
			fmap = &w->ww_fmap[i][j];
		for (; j < w->ww_i.r; j++) {
			if (*win & WWM_GLS) {
				win++;
				fmap++;
				continue;
			}
			*win++ |= WWM_GLS;
			if (w->ww_fmap)
				*fmap++ = 0;
			if (wwsmap[i][j] == w->ww_index) {
				wwsmap[i][j] = WWX_NOBODY;
				wwns[i][j].c_w = ' ';
				wwtouched[i] = 1;
			}
		}
		w->ww_nvis[i] = 0;
	}
	for (wp = w->ww_forw; wp != &wwhead; wp = wp->ww_forw) {
		int i1, i2, j1, j2;

		i1 = MAX(w->ww_i.t, wp->ww_i.t);
		i2 = MIN(w->ww_i.b, wp->ww_i.b);
		j1 = MAX(w->ww_i.l, wp->ww_i.l);
		j2 = MIN(w->ww_i.r, wp->ww_i.r);
		for (i = i1; i < i2; i++) {
			for (j = j1; j < j2; j++) {
				if (wwsmap[i][j] != WWX_NOBODY)
					continue;
				if ((wp->ww_win[i][j] & WWM_GLS) == 0) {
					wwsmap[i][j] = wp->ww_index;
					wwns[i][j].c_w = wp->ww_buf[i][j].c_w ^
						wp->ww_win[i][j] << WWC_MSHIFT;
					wwtouched[i] = 1;
					if (wp->ww_win[i][j] == 0)
						wp->ww_nvis[i]++;
				}
			}
		}
	}
}

#ifndef lint
static	char *sccsid = "@(#)wwadd.c	3.6 83/11/23";
#endif

#include "ww.h"

/*
 * Stick w1 behind w2.
 */
wwadd(w1, w2)
register struct ww *w1;
struct ww *w2;
{
	register struct ww *w;
	register i;

	w1->ww_order = w2->ww_order + 1;
	w1->ww_back = w2;
	w1->ww_forw = w2->ww_forw;
	w2->ww_forw->ww_back = w1;
	w2->ww_forw = w1;

	for (w = w1->ww_forw; w != &wwhead; w = w->ww_forw)
		w->ww_order++;
	for (i = w1->ww_i.t; i < w1->ww_i.b; i++) {
		register j = w1->ww_i.l;
		register char *smap = &wwsmap[i][j];
		register char *win = &w1->ww_win[i][j];
		int nvis = 0;

		for (j = w1->ww_i.l; j < w1->ww_i.r; j++) {
			w = wwindex[*smap];
			if (w1->ww_order > w->ww_order) {
				win++;
				smap++;
				continue;
			}
			if (*win & WWM_GLS) {
				win++;
				smap++;
				continue;
			}
			if (w != &wwnobody && w->ww_win[i][j] == 0)
				w->ww_nvis[i]--;
			*smap++ = w1->ww_index;
			if (*win == 0)
				nvis++;
			wwns[i][j].c_w = w1->ww_buf[i][j].c_w ^
				*win++ << WWC_MSHIFT;
			wwtouched[i] = 1;
		}
		w1->ww_nvis[i] = nvis;
	}
}

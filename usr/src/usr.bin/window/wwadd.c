#ifndef lint
static	char *sccsid = "@(#)wwadd.c	3.1 83/08/09";
#endif

#include "ww.h"

/*
 * Stick w1 behind w2
 * W1 should have an empty ww_cov map.
 */
wwadd(w1, w2)
register struct ww *w1, *w2;
{
	if (w1->ww_forw != 0 || w1->ww_back != 0)
		abort();
	w1->ww_order = w2->ww_order + 1;
	w1->ww_back = w2;
	w1->ww_forw = w2->ww_forw;
	w2->ww_forw->ww_back = w1;
	w2->ww_forw = w1;
	{
		register struct ww *wp;

		for (wp = w2; wp != &wwhead; wp = wp->ww_back)
			wwcover(wp, w1);
		for (wp = w1->ww_forw; wp != &wwhead; wp = wp->ww_forw) {
			wp->ww_order++;
			wwcover(w1, wp);
		}
	}
	{
		int i;

		for (i = w1->ww_w.t; i < w1->ww_w.b; i++) {
			int j;
			register char *win = w1->ww_win[i - w1->ww_w.t];
			register char *smap = &wwsmap[i][w1->ww_w.l];
			register union ww_char *ns = &wwns[i][w1->ww_w.l];
			register union ww_char *buf = w1->ww_buf[w1->ww_scroll
							+ i - w1->ww_w.t];

			for (j = w1->ww_w.nc; --j >= 0;) {
				if ((*win & (WWM_GLS|WWM_COV)) == 0) {
					*smap++ = w1->ww_index;
					ns++->c_w = buf++->c_w
						^ *win++ << WWC_MSHIFT;
				} else {
					smap++;
					ns++;
					win++;
					buf++;
				}
			}
		}
	}
}

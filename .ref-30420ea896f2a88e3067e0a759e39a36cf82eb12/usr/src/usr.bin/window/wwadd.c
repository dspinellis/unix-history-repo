#ifndef lint
static	char *sccsid = "@(#)wwadd.c	3.5 83/09/15";
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
		return;				/* sanity */

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
		int i = w1->ww_i.t;
		char *touched = &wwtouched[i];

		for (; i < w1->ww_i.b; i++, touched++) {
			int j = w1->ww_i.l;
			register char *win = &w1->ww_win[i][j];
			register char *smap = &wwsmap[i][j];
			register union ww_char *ns = &wwns[i][j];
			register union ww_char *buf = &w1->ww_buf[i][j];

			for (; j < w1->ww_i.r; j++) {
				if ((*win & (WWM_GLS|WWM_COV)) == 0) {
					*touched = 1;
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

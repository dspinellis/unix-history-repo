#ifndef lint
static	char *sccsid = "@(#)wwdelete.c	3.4 83/09/14";
#endif

#include "ww.h"

/*
 * Pull w free from the cover list.
 */
wwdelete(w)
register struct ww *w;
{
	if (w->ww_forw == 0 || w->ww_back == 0)
		return;				/* sanity */

	{
		register i = w->ww_i.t;
		register char *touched = &wwtouched[i];

		for (; i < w->ww_i.b; i++, touched++) {
			register int j = w->ww_i.l;
			register char *smap = &wwsmap[i][j];
			register union ww_char *ns = &wwns[i][j];

			for (j = w->ww_i.nc; --j >= 0;) {
				if (*smap == w->ww_index) {
					*touched = 1;
					*smap++ = WWX_NOBODY;
					ns++->c_w = ' ';
				} else {
					smap++;
					ns++;
				}
			}
		}
	}
	{
		register struct ww *wp;

		for (wp = w->ww_forw; wp != &wwhead; wp = wp->ww_forw) {
			wp->ww_order--;
			wwuncover(w, wp);
		}
	}
	{
		register i;

		for (i = w->ww_i.t; i < w->ww_i.b; i++) {
			register j;
			register char *win = &w->ww_win[i - w->ww_w.t]
					[w->ww_i.l - w->ww_w.l];
			register char *cov = &w->ww_cov[i - w->ww_w.t]
					[w->ww_i.l - w->ww_w.l];

			for (j = w->ww_i.nc; --j >= 0;) {
				if (*win != 0) {
					if ((*win++ &= ~WWM_COV) == 0)
						w->ww_nvis[i - w->ww_w.t]++;
				} else
					win++;
				*cov++ = WWX_NOBODY;
			}
		}
	}
	w->ww_back->ww_forw = w->ww_forw;
	w->ww_forw->ww_back = w->ww_back;
	w->ww_forw = w->ww_back = 0;
}

#ifndef lint
static	char *sccsid = "@(#)wwdelete.c	3.1 83/08/09";
#endif

#include "ww.h"

/*
 * Pull w free from the cover list.
 */
wwdelete(w)
register struct ww *w;
{
	register i;

	for (i = w->ww_w.t; i < w->ww_w.b; i++) {
		register int j = w->ww_w.l;
		register char *smap = &wwsmap[i][j];
		register union ww_char *ns = &wwns[i][j];
		for (j = w->ww_w.nc; --j >= 0;) {
			if (*smap == w->ww_index) {
				*smap++ = WWX_NOBODY;
				ns++->c_w = ' ';
			} else {
				smap++;
				ns++;
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
	for (i = 0; i < w->ww_w.nr; i++) {
		register j;
		register char *win = w->ww_win[i];
		register char *cov = w->ww_cov[i];
		for (j = w->ww_w.nc; --j >= 0;) {
			if (*win != 0) {
				if ((*win++ &= ~WWM_COV) == 0)
					w->ww_nvis[i]++;
			} else
				win++;
			*cov++ = WWX_NOBODY;
		}
	}
	w->ww_back->ww_forw = w->ww_forw;
	w->ww_forw->ww_back = w->ww_back;
	w->ww_forw = w->ww_back = 0;
}

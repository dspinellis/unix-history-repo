#ifndef lint
static	char *sccsid = "@(#)wwunframe.c	3.8 83/09/15";
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
	for (wp = w->ww_forw; wp != &wwhead; wp = wp->ww_forw)
		wwuncover(w, wp);
}

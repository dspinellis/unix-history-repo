#ifndef lint
static	char *sccsid = "@(#)wwunframe.c	3.10 83/11/23";
#endif

#include "ww.h"

wwunframe(w)
register struct ww *w;
{
	register i;

	for (i = w->ww_i.t; i < w->ww_i.b; i++) {
		register j = w->ww_i.l;
		register char *win = &w->ww_win[i][j];
		register char *fmap = w->ww_fmap ? &w->ww_fmap[i][j] : 0;
		register char *smap = &wwsmap[i][j];

		for (; j < w->ww_i.r; j++) {
			if (*win & WWM_GLS) {
				win++;
				fmap++;
				smap++;
				continue;
			}
			*win++ |= WWM_GLS;
			if (w->ww_fmap != 0)
				*fmap++ = 0;
			if (*smap == w->ww_index) {
				*smap = WWX_NOBODY;
				wwns[i][j].c_w = ' ';
				wwtouched[i] = 1;
			}
			smap++;
		}
		w->ww_nvis[i] = 0;
	}
	{
		register struct ww *wp;

		for (wp = w->ww_forw; wp != &wwhead; wp = wp->ww_forw)
			wwuncover(w, wp);
	}
}

#ifndef lint
static	char *sccsid = "@(#)wwunframe.c	3.6 83/08/18";
#endif

#include "ww.h"

wwunframe(w)
register struct ww *w;
{
	wwunframe1(w, 1);
}

wwunframe1(w, dofmap)
register struct ww *w;
char dofmap;
{
	register i, j;
	register char *win;
	register char *fmap;
	register struct ww *wp;

	for (i = w->ww_w.t; i < w->ww_w.b; i++) {
		win = w->ww_win[i - w->ww_w.t];
		fmap = wwfmap[i];
		for (j = w->ww_w.l; j < w->ww_w.r; j++) {
			if (*win & WWM_GLS) {
				win++;
				fmap++;
				continue;
			}
			*win++ |= WWM_GLS;
			if (dofmap)
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

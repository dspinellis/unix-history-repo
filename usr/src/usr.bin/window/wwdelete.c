#ifndef lint
static	char *sccsid = "@(#)wwdelete.c	3.7 83/11/23";
#endif

#include "ww.h"

/*
 * Pull w free from the cover list.
 */
wwdelete(w)
register struct ww *w;
{
	register struct ww *wp;
	register i, j;

	for (i = w->ww_i.t; i < w->ww_i.b; i++)
		for (j = w->ww_i.l; j < w->ww_i.r; j++)
			if (wwsmap[i][j] == w->ww_index) {
				wwsmap[i][j] = WWX_NOBODY;
				wwns[i][j].c_w = ' ';
				wwtouched[i] = 1;
			}

	for (wp = w->ww_forw; wp != &wwhead; wp = wp->ww_forw) {
		w->ww_order--;
		wwuncover(w, wp);
	}

	w->ww_back->ww_forw = w->ww_forw;
	w->ww_forw->ww_back = w->ww_back;
	w->ww_forw = w->ww_back = 0;
}

#ifndef lint
static	char *sccsid = "@(#)wwdelete.c	3.6 83/11/23";
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
		int i1, i2, j1, j2;

		w->ww_order--;
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

	w->ww_back->ww_forw = w->ww_forw;
	w->ww_forw->ww_back = w->ww_back;
	w->ww_forw = w->ww_back = 0;
}

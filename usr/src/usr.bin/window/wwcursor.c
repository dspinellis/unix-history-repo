#ifndef lint
static	char *sccsid = "@(#)wwcursor.c	3.1 83/08/19";
#endif

#include "ww.h"

wwcursor(w, on)
register struct ww *w;
{
	register char *win;
	register r, c;

	if (on) {
		if (w->ww_hascursor)
			return;
		w->ww_hascursor = 1;
	} else {
		if (!w->ww_hascursor)
			return;
		w->ww_hascursor = 0;
	}
	if (wwcursormodes != 0) {
		win = &w->ww_win[w->ww_cur.r][w->ww_cur.c];
		if (*win == 0)
			w->ww_nvis[w->ww_cur.r]--;
		else if (*win == wwcursormodes)
			w->ww_nvis[w->ww_cur.r]++;
		*win ^= wwcursormodes;
	}
	r = wwcurrow(w);
	c = wwcurcol(w);
	if (wwsmap[r][c] == w->ww_index) {
		wwns[r][c].c_m ^= wwcursormodes;
		wwtouched[r] = 1;
	}
}

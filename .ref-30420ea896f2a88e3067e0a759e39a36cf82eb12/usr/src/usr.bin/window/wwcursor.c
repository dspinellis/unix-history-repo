#ifndef lint
static	char *sccsid = "@(#)wwcursor.c	3.3 83/09/15";
#endif

#include "ww.h"

wwcursor(w, on)
register struct ww *w;
{
	register char *win;

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
		if (w->ww_cur.r < w->ww_i.t || w->ww_cur.r >= w->ww_i.b
		    || w->ww_cur.c < w->ww_i.l || w->ww_cur.c >= w->ww_i.r)
			return;
		if (wwsmap[w->ww_cur.r][w->ww_cur.c] == w->ww_index) {
			wwns[w->ww_cur.r][w->ww_cur.c].c_m ^= wwcursormodes;
			wwtouched[w->ww_cur.r] = 1;
		}
	}
}

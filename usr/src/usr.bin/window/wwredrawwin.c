#ifndef lint
static	char *sccsid = "@(#)wwredrawwin.c	3.4 83/09/14";
#endif

#include "ww.h"

wwredrawwin(w)
register struct ww *w;
{
	wwredrawwin1(w, w->ww_i.t - w->ww_w.t, w->ww_i.b - w->ww_w.t,
		w->ww_scroll);
}

wwredrawwin1(w, row1, row2, scroll)
register struct ww *w;
int row1, row2, scroll;
{
	int i;
	register j;
	register char *smap;
	register union ww_char *buf;
	register char *win;
	register union ww_char *ns;
	char *touched;

	touched = &wwtouched[row1 + w->ww_w.t];
	for (i = row1; i < row2; i++, touched++) {
		ns = &wwns[i + w->ww_w.t][w->ww_i.l];
		smap = &wwsmap[i + w->ww_w.t][w->ww_i.l];
		buf = &w->ww_buf[scroll + i][w->ww_i.l - w->ww_w.l];
		win = &w->ww_win[i][w->ww_i.l - w->ww_w.l];
		for (j = w->ww_i.nc; --j >= 0;)
			if (*smap++ != w->ww_index)
				win++, ns++, buf++;
			else {
				*touched = 1;
				ns++->c_w = buf++->c_w
					^ *win++ << WWC_MSHIFT;
			}
	}
}

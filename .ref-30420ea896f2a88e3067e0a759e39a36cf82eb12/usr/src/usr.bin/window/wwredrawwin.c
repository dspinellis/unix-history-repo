#ifndef lint
static	char *sccsid = "@(#)wwredrawwin.c	3.6 83/09/15";
#endif

#include "ww.h"

wwredrawwin1(w, row1, row2, offset)
register struct ww *w;
int row1, row2, offset;
{
	int i;
	register j;
	register char *smap;
	register union ww_char *buf;
	register char *win;
	register union ww_char *ns;
	char *touched;

	touched = &wwtouched[row1];
	for (i = row1; i < row2; i++, touched++) {
		ns = &wwns[i][w->ww_i.l];
		smap = &wwsmap[i][w->ww_i.l];
		buf = &w->ww_buf[i + offset][w->ww_i.l];
		win = &w->ww_win[i][w->ww_i.l];
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

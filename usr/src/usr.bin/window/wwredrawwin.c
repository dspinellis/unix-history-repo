#ifndef lint
static	char *sccsid = "@(#)wwredrawwin.c	3.2 83/08/16";
#endif

#include "ww.h"

/*			nobody calls it
wwredrawwin(w)
register struct ww *w;
{
	wwredrawwin1(w, 0, w->ww_w.nr - 1, w->ww_scroll);
}
*/

wwredrawwin1(w, srow, erow, offset)
register struct ww *w;
int srow, erow, offset;
{
	int i;
	register j;
	register char *smap;
	register union ww_char *buf;
	register char *win;
	register union ww_char *ns;
	char *touched;

	touched = &wwtouched[srow + w->ww_w.t];
	for (i = srow; i <= erow; i++, touched++) {
		ns = &wwns[i + w->ww_w.t][w->ww_w.l];
		smap = &wwsmap[i + w->ww_w.t][w->ww_w.l];
		buf = w->ww_buf[offset + i];
		win = w->ww_win[i];
		for (j = w->ww_w.nc; --j >= 0;)
			if (*smap++ != w->ww_index)
				win++, ns++, buf++;
			else {
				*touched = 1;
				ns++->c_w = buf++->c_w
					^ *win++ << WWC_MSHIFT;
			}
	}
}

#ifndef lint
static	char *sccsid = "@(#)wwmove.c	3.1 83/09/14";
#endif

#include "ww.h"

/*
 * Move a window.  Should be unattached.
 */
wwmove(w, row, col)
register struct ww *w;
{
	if (w->ww_forw != 0 || w->ww_back != 0)
		return;				/* sanity */

	w->ww_w.t = row;
	w->ww_w.b = w->ww_w.t + w->ww_w.nr;
	w->ww_w.l = col;
	w->ww_w.r = w->ww_w.l + w->ww_w.nc;

	w->ww_i.t = MAX(w->ww_w.t, 0);
	w->ww_i.b = MIN(w->ww_w.b, wwnrow);
	w->ww_i.nr = w->ww_i.b - w->ww_i.t;
	w->ww_i.l = MAX(w->ww_w.l, 0);
	w->ww_i.r = MIN(w->ww_w.r, wwncol);
	w->ww_i.nc = w->ww_i.r - w->ww_i.l;
}

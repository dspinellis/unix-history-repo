#ifndef lint
static	char *sccsid = "@(#)wwlabel.c	2.1 83/07/30";
#endif

#include "ww.h"

wwlabel(w, where, l, mode)
register struct ww *w;
register char *l;
{
	register i;
	register char *p;
	char ulc, top, urc, left, right, llc, bottom, lrc;

	if (w->ww_i.nrow == w->ww_w.nrow)	/* not framed */
		return -1;
	Wauxcursor(w->ww_win, 0, where);
	for (i = w->ww_o.ncol - where - 1; i > 0 && *l; l++)
		for (p = unctrl(*l); *p; p++, i--)
			Waputc(*p, mode, w->ww_win);
}

#ifndef lint
static	char *sccsid = "@(#)wwlabel.c	1.3 83/07/22";
#endif

#include "ww.h"

wwlabel(w, l, mode)
register struct ww *w;
register char *l;
{
	register i;
	char ulc, top, urc, left, right, llc, bottom, lrc;
	char framed;

	if (!(framed = w->ww_i.nrow < w->ww_o.nrow)) {
		Wgetframe(&ulc, &top, &urc, &left, &right, &llc, &bottom, &lrc);
		w->ww_i.nrow--;
		w->ww_i.row++;
	}
	Wauxcursor(w->ww_win, 0, 1);
	for (i = w->ww_o.ncol - 2; i > 0 && *l; i--, l++)
		Waputc(*l, mode, w->ww_win);
	if (!framed)
		for (; i > 0; i--)
			Waputc(top, 0, w->ww_win);
}

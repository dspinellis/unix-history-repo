#ifndef lint
static	char *sccsid = "@(#)wwlabel.c	1.2 83/07/19";
#endif

#include "ww.h"

wwlabel(w, l, mode)
register struct ww *w;
register char *l;
{
	register i;
	char ulc, top, urc, left, right, llc, bottom, lrc;
	char framed;

	if (!(framed = w->ww_inrow < w->ww_nrow)) {
		Wgetframe(&ulc, &top, &urc, &left, &right, &llc, &bottom, &lrc);
		w->ww_inrow--;
		w->ww_irow++;
	}
	Wauxcursor(w->ww_win, 0, 1);
	for (i = w->ww_ncol - 2; i > 0 && *l; i--, l++)
		Waputc(*l, mode, w->ww_win);
	if (!framed)
		for (; i > 0; i--)
			Waputc(top, 0, w->ww_win);
}

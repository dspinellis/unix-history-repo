#ifndef lint
static char sccsid[] = "@(#)wwlabel.c	3.13 %G%";
#endif

/*
 * Copyright (c) 1983 Regents of the University of California,
 * All rights reserved.  Redistribution permitted subject to
 * the terms of the Berkeley Software License Agreement.
 */

#include "ww.h"
#include "char.h"

wwlabel(w, where, l, mode)
struct ww *w;
register char *l;
{
	register i;
	int jj;
	char ulc, top, urc, left, right, llc, bottom, lrc;

	if (w->ww_i.nrow == w->ww_w.nrow)	/* not framed */
		return;
	Wauxcursor(w->ww_win, 0, where);
	for (i = w->ww_o.ncol - where - 1; i > 0 && *l; l++)
		for (p = unctrl(*l); *p; p++, i--)
			Waputc(*p, mode, w->ww_win);
}

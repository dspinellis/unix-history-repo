/*
 * Copyright (c) 1983 Regents of the University of California.
 * All rights reserved.
 *
 * Redistribution and use in source and binary forms are permitted
 * provided that this notice is preserved and that due credit is given
 * to the University of California at Berkeley. The name of the University
 * may not be used to endorse or promote products derived from this
 * software without specific prior written permission. This software
 * is provided ``as is'' without express or implied warranty.
 */

#ifndef lint
static char sccsid[] = "@(#)wwlabel.c	3.14 (Berkeley) %G%";
#endif /* not lint */

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

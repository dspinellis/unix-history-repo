/*
 * Copyright (c) 1983 Regents of the University of California.
 * All rights reserved.
 *
 * Redistribution and use in source and binary forms are permitted
 * provided that the above copyright notice and this paragraph are
 * duplicated in all such forms and that any documentation,
 * advertising materials, and other materials related to such
 * distribution and use acknowledge that the software was developed
 * by the University of California, Berkeley.  The name of the
 * University may not be used to endorse or promote products derived
 * from this software without specific prior written permission.
 * THIS SOFTWARE IS PROVIDED ``AS IS'' AND WITHOUT ANY EXPRESS OR
 * IMPLIED WARRANTIES, INCLUDING, WITHOUT LIMITATION, THE IMPLIED
 * WARRANTIES OF MERCHANTIBILITY AND FITNESS FOR A PARTICULAR PURPOSE.
 */

#ifndef lint
static char sccsid[] = "@(#)wwlabel.c	3.15 (Berkeley) %G%";
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

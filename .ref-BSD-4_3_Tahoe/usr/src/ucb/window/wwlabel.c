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
static char sccsid[] = "@(#)wwlabel.c	3.15 (Berkeley) 6/29/88";
#endif /* not lint */

#include "ww.h"
#include "char.h"

/*
 * Label window w on f,
 * at 1 line above w and 'where' columns from it's left edge.
 * Gross, but it works.
 */
wwlabel(w, f, where, l, mode)
struct ww *w;
struct ww *f;
char *l;
{
	int row;
	register j;
	int jj;
	register char *win;
	register union ww_char *buf;
	register union ww_char *ns;
	register char *fmap;
	register char *smap;
	char touched;
	char *p;

	if (f->ww_fmap == 0)
		return;

	row = w->ww_w.t - 1;
	if (row < f->ww_i.t || row >= f->ww_i.b)
		return;
	win = f->ww_win[row];
	buf = f->ww_buf[row];
	fmap = f->ww_fmap[row];
	ns = wwns[row];
	smap = wwsmap[row];
	touched = wwtouched[row];
	mode <<= WWC_MSHIFT;

	jj = MIN(w->ww_i.r, f->ww_i.r);
	j = w->ww_i.l + where;
	while (j < jj && *l)
		for (p = unctrl(*l++); j < jj && *p; j++, p++) {
			/* can't label if not already framed */
			if (win[j] & WWM_GLS)
				continue;
			if (smap[j] != f->ww_index)
				buf[j].c_w = mode | *p;
			else {
				ns[j].c_w = (buf[j].c_w = mode | *p)
						^ win[j] << WWC_MSHIFT;
				touched |= WWU_TOUCHED;
			}
			fmap[j] |= WWF_LABEL;
		}
	wwtouched[row] = touched;
}

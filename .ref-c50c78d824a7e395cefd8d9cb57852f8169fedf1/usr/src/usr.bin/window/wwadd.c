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
static char sccsid[] = "@(#)wwadd.c	3.12 (Berkeley) %G%";
#endif /* not lint */

#include "ww.h"

/*
 * Stick w1 behind w2.
 */
wwadd(w1, w2)
register struct ww *w1;
struct ww *w2;
{
	register i;
	register struct ww *w;

	w1->ww_order = w2->ww_order + 1;
	w1->ww_back = w2;
	w1->ww_forw = w2->ww_forw;
	w2->ww_forw->ww_back = w1;
	w2->ww_forw = w1;

	for (w = w1->ww_forw; w != &wwhead; w = w->ww_forw)
		w->ww_order++;
	for (i = w1->ww_i.t; i < w1->ww_i.b; i++) {
		register j;
		register char *smap = wwsmap[i];
		register char *win = w1->ww_win[i];
		union ww_char *ns = wwns[i];
		union ww_char *buf = w1->ww_buf[i];
		int nvis = 0;
		int nchanged = 0;

		for (j = w1->ww_i.l; j < w1->ww_i.r; j++) {
			w = wwindex[smap[j]];
			if (w1->ww_order > w->ww_order)
				continue;
			if (win[j] & WWM_GLS)
				continue;
			if (w != &wwnobody && w->ww_win[i][j] == 0)
				w->ww_nvis[i]--;
			smap[j] = w1->ww_index;
			if (win[j] == 0)
				nvis++;
			ns[j].c_w = buf[j].c_w ^ win[j] << WWC_MSHIFT;
			nchanged++;
		}
		if (nchanged > 0)
			wwtouched[i] |= WWU_TOUCHED;
		w1->ww_nvis[i] = nvis;
	}
}

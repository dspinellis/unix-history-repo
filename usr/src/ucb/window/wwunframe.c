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
static char sccsid[] = "@(#)wwunframe.c	3.17 (Berkeley) 6/29/88";
#endif /* not lint */

#include "ww.h"

wwunframe(w)
register struct ww *w;
{
	int i;

	for (i = w->ww_i.t; i < w->ww_i.b; i++) {
		register j;
		register char *win = w->ww_win[i];
		register char *fmap = w->ww_fmap ? w->ww_fmap[i] : 0;
		register char *smap = wwsmap[i];
		register union ww_char *ns = wwns[i];
		int nchanged = 0;

		for (j = w->ww_i.l; j < w->ww_i.r; j++) {
			if (win[j] & WWM_GLS)
				continue;
			win[j] |= WWM_GLS;
			if (fmap != 0)
				fmap[j] = 0;
			if (smap[j] == w->ww_index) {
				smap[j] = WWX_NOBODY;
				ns[j].c_w = ' ';
				nchanged++;
			}
		}
		if (nchanged > 4)
			wwtouched[i] |= WWU_MAJOR|WWU_TOUCHED;
		else if (nchanged > 0)
			wwtouched[i] |= WWU_TOUCHED;
		w->ww_nvis[i] = 0;
	}

	if (w->ww_forw != &wwhead)
		wwdelete1(w->ww_forw,
			w->ww_i.t, w->ww_i.b, w->ww_i.l, w->ww_i.r);
}

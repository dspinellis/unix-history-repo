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
static char sccsid[] = "@(#)wwunframe.c	3.16 (Berkeley) %G%";
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

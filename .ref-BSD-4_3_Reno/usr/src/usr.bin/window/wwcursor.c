/*
 * Copyright (c) 1983 Regents of the University of California.
 * All rights reserved.
 *
 * This code is derived from software contributed to Berkeley by
 * Edward Wang at The University of California, Berkeley.
 *
 * Redistribution and use in source and binary forms are permitted provided
 * that: (1) source distributions retain this entire copyright notice and
 * comment, and (2) distributions including binaries display the following
 * acknowledgement:  ``This product includes software developed by the
 * University of California, Berkeley and its contributors'' in the
 * documentation or other materials provided with the distribution and in
 * all advertising materials mentioning features or use of this software.
 * Neither the name of the University nor the names of its contributors may
 * be used to endorse or promote products derived from this software without
 * specific prior written permission.
 * THIS SOFTWARE IS PROVIDED ``AS IS'' AND WITHOUT ANY EXPRESS OR IMPLIED
 * WARRANTIES, INCLUDING, WITHOUT LIMITATION, THE IMPLIED WARRANTIES OF
 * MERCHANTABILITY AND FITNESS FOR A PARTICULAR PURPOSE.
 */

#ifndef lint
static char sccsid[] = "@(#)wwcursor.c	3.12 (Berkeley) 6/6/90";
#endif /* not lint */

#include "ww.h"

wwcursor(w, on)
register struct ww *w;
{
	register char *win;

	if (on) {
		if (w->ww_hascursor)
			return;
		w->ww_hascursor = 1;
	} else {
		if (!w->ww_hascursor)
			return;
		w->ww_hascursor = 0;
	}
	if (wwcursormodes != 0) {
		win = &w->ww_win[w->ww_cur.r][w->ww_cur.c];
		*win ^= wwcursormodes;
		if (w->ww_cur.r < w->ww_i.t || w->ww_cur.r >= w->ww_i.b
		    || w->ww_cur.c < w->ww_i.l || w->ww_cur.c >= w->ww_i.r)
			return;
		if (wwsmap[w->ww_cur.r][w->ww_cur.c] == w->ww_index) {
			if (*win == 0)
				w->ww_nvis[w->ww_cur.r]++;
			else if (*win == wwcursormodes)
				w->ww_nvis[w->ww_cur.r]--;
			wwns[w->ww_cur.r][w->ww_cur.c].c_m ^= wwcursormodes;
			wwtouched[w->ww_cur.r] |= WWU_TOUCHED;
		}
	}
}

wwsetcursormodes(new)
register new;
{
	register i;
	register struct ww *w;
	register old = wwcursormodes;

	new &= wwavailmodes;
	if (new == wwcursormodes)
		return;
	for (i = 0; i < NWW; i++)
		if (wwindex[i] != 0 && (w = wwindex[i])->ww_hascursor) {
			wwcursor(w, 0);
			wwcursormodes = new;
			wwcursor(w, 1);
			wwcursormodes = old;
		}
	wwcursormodes = new;
}

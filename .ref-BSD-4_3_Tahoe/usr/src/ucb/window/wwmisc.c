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
static char sccsid[] = "@(#)wwmisc.c	3.12 (Berkeley) 6/29/88";
#endif /* not lint */

#include "ww.h"
#include "tt.h"
#include "char.h"

/*
 * Sufficient but not necessary test for total visibility.
 */
wwvisible(w)
register struct ww *w;
{
	register i;
	register nvis = 0;

	for (i = w->ww_i.t; i < w->ww_i.b; i++)
		nvis += w->ww_nvis[i];
	if (w->ww_hascursor
	    && w->ww_cur.r >= w->ww_i.t && w->ww_cur.r < w->ww_i.b
	    && w->ww_cur.c >= w->ww_i.l && w->ww_cur.c < w->ww_i.r
	    && wwsmap[w->ww_cur.r][w->ww_cur.c] == w->ww_index)
		nvis++;
	return nvis == w->ww_i.nr * w->ww_i.nc;
}

wwbell()
{
	ttputc(ctrl('g'));
}

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
static char sccsid[] = "@(#)wwclreol.c	3.20 (Berkeley) 6/6/90";
#endif /* not lint */

#include "ww.h"
#include "tt.h"

/*
 * Clear w to the end of line.
 * If cleared is true, then the screen line has already been cleared.
 */
wwclreol1(w, row, col, cleared)
register struct ww *w;
int row, col;
char cleared;
{
	register i;

	/*
	 * Clear the buffer right off
	 */
	{
		register union ww_char *buf;

		buf = &w->ww_buf[row][col]; 
		for (i = w->ww_b.r - col; --i >= 0;)
			buf++->c_w = ' ';
	}

	/*
	 * If can't see it, just return.
	 */
	if (row < w->ww_i.t || row >= w->ww_i.b
	    || w->ww_i.r <= 0 || w->ww_i.r <= col)
		return;

	if (col < w->ww_i.l)
		col = w->ww_i.l;

	/*
	 * Now fix wwns.
	 */
	{
		register union ww_char *s;
		register char *smap, *win;

		i = col;
		smap = &wwsmap[row][i];
		s = &wwns[row][i];
		win = &w->ww_win[row][i];
		for (i = w->ww_i.r - i; --i >= 0;)
			if (*smap++ == w->ww_index)
				s++->c_w = ' ' | *win++ << WWC_MSHIFT;
			else
				s++, win++;
	}
	if (!cleared)
		wwtouched[row] |= WWU_TOUCHED;
}

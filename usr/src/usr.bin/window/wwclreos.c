#ifndef lint
static char sccsid[] = "@(#)wwclreos.c	3.6 %G%";
#endif

/*
 * Copyright (c) 1983 Regents of the University of California,
 * All rights reserved.  Redistribution permitted subject to
 * the terms of the Berkeley Software License Agreement.
 */

#include "ww.h"
#include "tt.h"

wwclreos(w, row, col)
register struct ww *w;
{
	register i;
	int cleared = 0;

	/*
	 * Quick and dirty check for windows that cover the bottom
	 * portion of the screen.  Not meant to be complete.
	 */
	if (tt.tt_clreos && w->ww_i.b == wwnrow && w->ww_i.l == 0 &&
	    w->ww_i.r == wwncol && wwvisible(w)) {
		register j;
		register union ww_char *s;

		i = row;
		(*tt.tt_move)(i, col);
		(*tt.tt_clreos)();
		/*
		 * We have to fix wwos becuase wwclreol1 won't do that.
		 */
		s = &wwos[i][col];
		for (j = wwncol - col; --j >= 0;)
			s++->c_w = ' ';
		for (i++; i < wwnrow; i++) {
			s = wwos[i];
			for (j = wwncol; --j >= 0;)
				s++->c_w = ' ';
		}
		cleared = 1;
	}
	wwclreol1(w, row, col, cleared);
	for (i = row + 1; i < w->ww_b.b; i++)
		wwclreol1(w, i, w->ww_b.l, cleared);
}

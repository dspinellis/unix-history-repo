#ifndef lint
static char sccsid[] = "@(#)wwclreos.c	3.5 4/24/85";
#endif

/*
 * Copyright (c) 1983 Regents of the University of California,
 * All rights reserved.  Redistribution permitted subject to
 * the terms of the Berkeley Software License Agreement.
 */

#include "ww.h"

wwclreos(w, row, col)
register struct ww *w;
{
	register i;

	wwclreol(w, row, col);
	for (i = row + 1; i < w->ww_b.b; i++)
		wwclreol(w, i, w->ww_b.l);
}

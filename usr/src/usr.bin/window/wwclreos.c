/*
 * Copyright (c) 1983 Regents of the University of California.
 * All rights reserved.
 *
 * This code is derived from software contributed to Berkeley by
 * Edward Wang at The University of California, Berkeley.
 *
 * %sccs.include.redist.c%
 */

#ifndef lint
static char sccsid[] = "@(#)wwclreos.c	3.11 (Berkeley) %G%";
#endif /* not lint */

#include "ww.h"
#include "tt.h"

wwclreos(w, row, col)
register struct ww *w;
{
	register i;

	wwclreol(w, row, col);
	for (i = row + 1; i < w->ww_b.b; i++)
		wwclreol(w, i, w->ww_b.l);
	/* XXX */
	if (!w->ww_noupdate)
		wwupdate1(w->ww_i.t, w->ww_i.b);
}

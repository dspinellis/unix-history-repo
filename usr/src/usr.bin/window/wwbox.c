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
static char sccsid[] = "@(#)wwbox.c	3.8 (Berkeley) %G%";
#endif /* not lint */

#include "ww.h"
#include "tt.h"

wwbox(w, r, c, nr, nc)
register struct ww *w;
register r, c;
int nr, nc;
{
	register r1, c1;
	register i;

	r1 = r + nr - 1;
	c1 = c + nc - 1;
	wwframec(w, r, c, WWF_D|WWF_R);
	for (i = c + 1; i < c1; i++)
		wwframec(w, r, i, WWF_L|WWF_R);
	wwframec(w, r, i, WWF_L|WWF_D);
	for (i = r + 1; i < r1; i++)
		wwframec(w, i, c1, WWF_U|WWF_D);
	wwframec(w, i, c1, WWF_U|WWF_L);
	for (i = c1 - 1; i > c; i--)
		wwframec(w, r1, i, WWF_R|WWF_L);
	wwframec(w, r1, i, WWF_R|WWF_U);
	for (i = r1 - 1; i > r; i--)
		wwframec(w, i, c, WWF_D|WWF_U);
}

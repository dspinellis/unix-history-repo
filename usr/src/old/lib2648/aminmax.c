/*
 * Copyright (c) 1980 Regents of the University of California.
 * All rights reserved.  The Berkeley software License Agreement
 * specifies the terms and conditions for redistribution.
 */

#ifndef lint
static char sccsid[] = "@(#)aminmax.c	5.1 (Berkeley) %G%";
#endif not lint

/*
 * aminmax: find the 4 edges of the glyph within a window.
 * This version is approximate, in that it may include some
 * blank areas.  But it's much faster because it doesn't have
 * to call mat over and over.
 */

#include "bit.h"

aminmax(g, nrow, ncol, rmin, cmin, rmax, cmax)
bitmat g;
int nrow, ncol;
int *rmin, *cmin, *rmax, *cmax;
{
	register int i, j;
	register int nc = (ncol+7)>>3;
	register int r1, r2, c1, c2;

	r1 = nrow; c1 = nc; r2 = c2 = 0;
	for (i=0; i<nrow; i++)
		for (j=0; j<nc; j++)
			if (g[i*nc+j]) {
				r1 = min(r1, i);
				r2 = max(r2, i);
				c1 = min(c1, j);
				c2 = max(c2, j);
			}
	if (r2 < r1) {
		/* empty glyph! */
		r1 = c1 = r2 = c2 = 1;
	}
	*rmin = r1; *rmax = r2;
	*cmin = 8*c1; *cmax = 8*c2+7;
}

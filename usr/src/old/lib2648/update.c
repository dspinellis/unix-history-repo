/*
 * Copyright (c) 1980 Regents of the University of California.
 * All rights reserved.  The Berkeley software License Agreement
 * specifies the terms and conditions for redistribution.
 */

#ifndef lint
static char sccsid[] = "@(#)update.c	5.1 (Berkeley) %G%";
#endif not lint

/*
 * update: the key output optimization routine of the whole editor.
 * The input consists of two bit matrices (mold is what's on the screen,
 * mnew is what we want to be on the screen) and the coordinates of
 * the lower left corner on the screen where this matrix is.
 * This routine does whatever is necessary to get the screen to look
 * like mnew, assuming that it currently looks like mold.
 *
 * (If I could patent this process for bread and other food I
 * would be a rich man.)
 */

#include "bit.h"

update(mold, mnew, rows, cols, baser, basec)
bitmat mold, mnew;
int rows, cols, baser, basec;
{
	int irow;
	register int i, j, k;
	int r1, r2, c1, c2, nr1, nr2, nc1, nc2;
	extern int QUIET;

#ifdef TRACE
	if (trace)
		fprintf(trace, "update(mold=%x, mnew=%x, rows=%d, cols=%d, baser=%d, basec=%d)\n", mold, mnew, rows, cols, baser, basec);
#endif

	if (QUIET)
		return;
	aminmax(mold, rows, cols, &r1, &c1, &r2, &c2);
	aminmax(mnew, rows, cols, &nr1, &nc1, &nr2, &nc2);
	r1 = min(r1, nr1); r2 = max(r2, nr2);
	c1 = min(c1, nc1); c2 = max(c2, nc2);

	dumpmat("mold:", mold, rows, cols);
	dumpmat("mnew:", mnew, rows, cols);

	for (i=r1; i<=r2; i++) {
		irow = baser + rows - i - 1;
		if (emptyrow(mnew, rows, cols, i)) {
			if (emptyrow(mold, rows, cols, i)) {
				continue;	/* identically blank. skip. */
			}
			/*
			 * Row i is to be cleared.  Look for some more
			 * rows to clear and do it all at once.
			 */
			for (j=i+1; j<rows && emptyrow(mnew,rows,cols,j); j++)
				;
			areaclear(baser+rows-j, basec, irow, basec+cols-1);
			i = j-1;	/* skip the others */
		} else for (j=c1; j<=c2; j++) {
			/*
			 * Result row is not all blank.  We look for stretches
			 * of bits that have to be changed (in either
			 * direction) and draw an exclusive or line over all
			 * the bits in each stretch.
			 */
			if (mat(mold,rows,cols,i,j,1)!=mat(mnew,rows,cols,i,j,2)){
				for (k=j+1; k<cols && mat(mold,rows,cols,i,k,3)!=
					mat(mnew,rows,cols,i,k,4); k++)
					;
				k--;
				setxor();
				line(basec+j, irow, basec+k, irow);
				j = k;	/* skip the others */
			}
		}
	}
}

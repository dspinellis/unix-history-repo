/*	dumpmat.c	4.2	83/06/10	*/

#include "bit.h"

#ifdef TRACE
/*
 * dumpmat: debugging dumpmat of a window or other bit matrix.
 * msg is a handy label, m is the matrix, rows, cols is the size of the matrix.
 */
dumpmat(msg, m, rows, cols)
char *msg;
bitmat m;
int rows, cols;
{
	register int r, c;
	int r1, r2, c1, c2;

	if (trace == NULL)
		return;
	fprintf(trace, "\ndumpmat %s, m=%x, rows=%d, cols=%d\n", msg, m, rows, cols);
	minmax(m, rows, cols, &r1, &c1, &r2, &c2);
	fprintf(trace, "r1=%d, r2=%d, c1=%d, c2=%d\n", r1, r2, c1, c2);
	for (r=r1; r<=r2; r++) {
		fprintf(trace, "%2d ", r);
		for (c=c1; c<=c2; c++)
			fprintf(trace, "%c", mat(m, rows, cols, r, c, 5) ? 'X' : '.');
		fprintf(trace, "\n");
	}
	fprintf(trace, "\n");
}
#endif

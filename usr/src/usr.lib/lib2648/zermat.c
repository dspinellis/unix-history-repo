/*	zermat.c	4.1	83/03/09	*/
/*
 * zermat: set a matrix to all zeros
 */

#include "bit.h"

zermat(m, rows, cols)
bitmat m;
int rows, cols;
{
	register int size = ((cols + 7) >> 3) * rows;
	register char *p;

	for (p = &m[size]; p>=m; )
		*--p = 0;
}

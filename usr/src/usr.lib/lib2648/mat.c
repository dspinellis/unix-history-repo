/*	mat.c	4.1	83/03/09	*/
/*
 * mat: retrieve the value in m[r, c].
 * rows and cols are the size of the matrix in all these routines.
 */

#include "bit.h"

int
mat(m, rows, cols, r, c)
register bitmat m;
register int c;
int rows, cols, r;
{
	register int thisbyte;

	thisbyte = m[r*((cols+7)>>3) + (c>>3)] & 0xff;
	thisbyte &= 0x80 >> (c&7);
	return (thisbyte != 0);
}

/*
 * Copyright (c) 1980 Regents of the University of California.
 * All rights reserved.  The Berkeley software License Agreement
 * specifies the terms and conditions for redistribution.
 */

#ifndef lint
static char sccsid[] = "@(#)zermat.c	5.1 (Berkeley) %G%";
#endif not lint

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

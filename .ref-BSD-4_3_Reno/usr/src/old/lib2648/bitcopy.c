/*
 * Copyright (c) 1980 Regents of the University of California.
 * All rights reserved.  The Berkeley software License Agreement
 * specifies the terms and conditions for redistribution.
 */

#ifndef lint
static char sccsid[] = "@(#)bitcopy.c	5.1 (Berkeley) 4/26/85";
#endif not lint

/*
 * Copy from msrc to mdest.
 * This is done as it is because it would be much slower to do it
 * a bit at a time.
 */

#include "bit.h"

bitcopy(mdest, msrc, rows, cols)
bitmat mdest, msrc;
int rows, cols;
{
	register int size = ((cols + 7) >> 3) * rows;
	register char *p, *q;

	for (p = &mdest[size], q = &msrc[size]; p>=mdest; )
		*--p = *--q;
}

/*
 * Copyright (c) 1980 Regents of the University of California.
 * All rights reserved.  The Berkeley software License Agreement
 * specifies the terms and conditions for redistribution.
 */

#ifndef lint
static char sccsid[] = "@(#)emptyrow.c	5.1 (Berkeley) %G%";
#endif not lint

/*
 * emptyrow: returns true if row r of m is all zeros.
 *
 * Note that we assume the garbage at the end of the
 * row is all zeros.
 */

#include "bit.h"

emptyrow(m, rows, cols, r)
bitmat m;
int rows, cols, r;
{
	char *top, *bot;

	bot = &m[r*((cols+7)>>3)];
	top = bot + ((cols-1) >> 3);
	while (bot <= top)
		if (*bot++)
			return(0);
	return (1);
}

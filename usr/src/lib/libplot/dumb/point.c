/*-
 * Copyright (c) 1980, 1993
 *	The Regents of the University of California.  All rights reserved.
 *
 * %sccs.include.proprietary.c%
 */

#ifndef lint
static char sccsid[] = "@(#)point.c	8.1 (Berkeley) %G%";
#endif /* not lint */

#include "dumb.h"

point(x, y)
	int x,y;
{
	scale(x, y);
	currentx = x;
	currenty = y;
	screenmat[currentx][currenty] = '*';
}

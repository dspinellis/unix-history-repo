/*-
 * Copyright (c) 1980, 1993
 *	The Regents of the University of California.  All rights reserved.
 *
 * This module is believed to contain source code proprietary to AT&T.
 * Use and redistribution is subject to the Berkeley Software License
 * Agreement and your Software Agreement with AT&T (Western Electric).
 */

#ifndef lint
static char sccsid[] = "@(#)move.c	8.1 (Berkeley) 6/4/93";
#endif /* not lint */

#include "gigi.h"

move(xi,yi)
int xi,yi;
{
	currentx = xsc(xi);
	currenty = ysc(yi);
	printf("P[%d,%d]",currentx, currenty);
}

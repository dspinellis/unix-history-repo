/*-
 * Copyright (c) 1980, 1986, 1993
 *	The Regents of the University of California.  All rights reserved.
 *
 * This module is believed to contain source code proprietary to AT&T.
 * Use and redistribution is subject to the Berkeley Software License
 * Agreement and your Software Agreement with AT&T (Western Electric).
 */

#ifndef lint
static char sccsid[] = "@(#)circle.c	8.1 (Berkeley) 6/4/93";
#endif /* not lint */

#include "grnplot.h"

/*---------------------------------------------------------
 *	Circle draws a circle.
 *
 *	Results:	None.
 *
 *	Side Effects:
 *	A circle of radius r is drawn at (x,y).
 *	The current position is set to (x,y);
 *---------------------------------------------------------
 */
circle(x, y, r)
int x, y, r;
{
	if (!ingrnfile) erase();
	endvector();
	printf("ARC\n");
	outxy(x,y);
	outxy(x+r,y);
	outxy(x,y+r);
	outxy(x,y-r);
	outxy(x+r,y);
	outxy(x-r,y);
	printf("*\n%d 0\n0\n",linestyle);
	curx=x;
	cury=y;
}

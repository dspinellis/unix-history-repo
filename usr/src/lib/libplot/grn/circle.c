/*-
 * Copyright (c) 1980, 1986, 1993
 *	The Regents of the University of California.  All rights reserved.
 *
 * %sccs.include.proprietary.c%
 */

#ifndef lint
static char sccsid[] = "@(#)circle.c	8.1 (Berkeley) %G%";
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

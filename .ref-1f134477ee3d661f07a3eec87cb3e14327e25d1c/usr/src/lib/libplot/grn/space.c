/*-
 * Copyright (c) 1980, 1986, 1993
 *	The Regents of the University of California.  All rights reserved.
 *
 * %sccs.include.proprietary.c%
 */

#ifndef lint
static char sccsid[] = "@(#)space.c	8.1 (Berkeley) %G%";
#endif /* not lint */

#include "grnplot.h"

/*---------------------------------------------------------
 *	Space sets up the world-to-screen transformation so
 *	that the rectangular area described by (x0, y0) and
 *	(x1, y1) will all be on-screen.
 *
 *	Results:	None.
 *
 *	Side Effects:
 *	Our own variables scale, xbot, and ybot are changed.
 *---------------------------------------------------------
 */
space(x0, y0, x1, y1)
int x0, y0, x1, y1;
{
    double xscale=0.0, yscale=0.0;
    if (x1>x0)
	    xscale = GRXMAX/(double)(x1-x0);
    if (y1>y0)
	    yscale = GRYMAX/(double)(y1-y0);
    scale = (xscale > yscale && yscale > 0)? yscale : xscale;
    if (scale == 0.0) scale == 1.0;
    xbot = x0;
    ybot = y0;
}

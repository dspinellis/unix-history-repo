/*-
 * Copyright (c) 1980, 1986, 1993
 *	The Regents of the University of California.  All rights reserved.
 *
 * This module is believed to contain source code proprietary to AT&T.
 * Use and redistribution is subject to the Berkeley Software License
 * Agreement and your Software Agreement with AT&T (Western Electric).
 */

#ifndef lint
static char sccsid[] = "@(#)move.c	8.1 (Berkeley) 6/4/93";
#endif /* not lint */

#include "grnplot.h"

/*---------------------------------------------------------
 *	This routine moves the current point to (x,y).
 *
 *	Results:	None.
 *	Side Effects:	If current line, close it.
 *---------------------------------------------------------
 */
move(x, y)
int x, y;
{
	if (!ingrnfile) erase();
    if (invector) endvector();
    curx = x;
    cury = y;
}

/*-
 * Copyright (c) 1980, 1986 The Regents of the University of California.
 * All rights reserved.
 *
 * %sccs.include.proprietary.c%
 */

#ifndef lint
static char sccsid[] = "@(#)move.c	6.2 (Berkeley) %G%";
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

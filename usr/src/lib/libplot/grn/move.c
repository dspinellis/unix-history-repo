/*
 * Copyright (c) 1980, 1986 Regents of the University of California.
 * All rights reserved.  The Berkeley software License Agreement
 * specifies the terms and conditions for redistribution.
 */

#ifndef lint
static char sccsid[] = "@(#)move.c	6.1 (Berkeley) %G%";
#endif not lint


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

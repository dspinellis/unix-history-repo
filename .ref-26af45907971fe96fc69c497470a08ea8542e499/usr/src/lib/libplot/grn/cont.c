/*-
 * Copyright (c) 1980, 1986, 1993
 *	The Regents of the University of California.  All rights reserved.
 *
 * %sccs.include.proprietary.c%
 */

#ifndef lint
static char sccsid[] = "@(#)cont.c	8.1 (Berkeley) %G%";
#endif /* not lint */

#include "grnplot.h"

/*---------------------------------------------------------
 *	Cont plots a line between (curx, cury) and (x, y).
 *
 *	Results:	None.
 *	Side Effects:	As above.
 *---------------------------------------------------------
 */
cont(x, y)
int x, y;
{
    startvector();
    curx = x;
    cury = y;
    outcurxy();
}

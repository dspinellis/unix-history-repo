/*-
 * Copyright (c) 1980, 1986 The Regents of the University of California.
 * All rights reserved.
 *
 * %sccs.include.proprietary.c%
 */

#ifndef lint
static char sccsid[] = "@(#)close.c	6.2 (Berkeley) %G%";
#endif /* not lint */

#include "grnplot.h"

/*---------------------------------------------------------
 *	Closepl ends the gremlin file
 *
 *	Results:	None.
 *
 *	Side Effects:
 *---------------------------------------------------------
 */
closepl()
{
    if (!ingrnfile) return;
    endvector();
    printf("-1\n");
    ingrnfile = 0;
}

/*
 * Copyright (c) 1980, 1986 Regents of the University of California.
 * All rights reserved.  The Berkeley software License Agreement
 * specifies the terms and conditions for redistribution.
 */

#ifndef lint
static char sccsid[] = "@(#)close.c	6.1 (Berkeley) %G%";
#endif not lint


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

/*-
 * Copyright (c) 1980, 1986, 1993
 *	The Regents of the University of California.  All rights reserved.
 *
 * %sccs.include.proprietary.c%
 */

#ifndef lint
static char sccsid[] = "@(#)erase.c	8.1 (Berkeley) %G%";
#endif /* not lint */

#include "grnplot.h"

/*---------------------------------------------------------
 *	This routine erases the screen.
 *
 *	Results:	None.
 *	Side Effects:	A new grn file is begun
 *	but: it is concatentated to the old one.
 *---------------------------------------------------------
 */
erase()
{
	if (ingrnfile)
	{
		closepl();
		fputs("multiple grn files in output must be separated by hand!\n",stderr);
	}
    printf("sungremlinfile\n0.00 0.00\n");
    ingrnfile = 1;
    invector = 0;
    scale = 1;
    curx = cury = xbot = ybot = 0;
}

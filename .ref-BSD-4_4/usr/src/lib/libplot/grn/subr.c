/*-
 * Copyright (c) 1980, 1986, 1993
 *	The Regents of the University of California.  All rights reserved.
 *
 * This module is believed to contain source code proprietary to AT&T.
 * Use and redistribution is subject to the Berkeley Software License
 * Agreement and your Software Agreement with AT&T (Western Electric).
 */

#ifndef lint
static char sccsid[] = "@(#)subr.c	8.1 (Berkeley) 6/4/93";
#endif /* not lint */

#include "grnplot.h"


/*---------------------------------------------------------
 *	This local routine outputs an x-y coordinate pair in the standard
 *	format required by the grn file.
 *
 *	Results:	None.
 *	
 *	Side Effects:
 *
 *	Errors:		None.
 *---------------------------------------------------------
 */
outxy(x, y)
int x, y;			/* The coordinates to be output.  Note:
				 * these are world coordinates, not screen
				 * ones.  We scale in this routine.
				 */
{
    printf("%.2f %.2f\n", (x - xbot)*scale,(y - ybot)*scale);
}

outcurxy()
{
	outxy(curx,cury);
}

startvector()
{
	if (!ingrnfile) erase();
	if (invector) return;
	invector = 1;
	printf("VECTOR\n");
	outcurxy();
}

endvector()
{
	if (!invector) return;
	invector = 0;
	printf("*\n%d 0\n0\n",linestyle);
}

/*-
 * Copyright (c) 1980, 1993
 *	The Regents of the University of California.  All rights reserved.
 *
 * This module is believed to contain source code proprietary to AT&T.
 * Use and redistribution is subject to the Berkeley Software License
 * Agreement and your Software Agreement with AT&T (Western Electric).
 */

#ifndef lint
static char sccsid[] = "@(#)line.c	8.1 (Berkeley) 6/4/93";
#endif /* not lint */

#include "gigi.h"

line(x0,y0,x1,y1)
int x0,y0,x1,y1;
{
	if(xsc(x0)==currentx && ysc(y0)==currenty)
		cont(x1,y1);
	else if(xsc(x1)==currentx && ysc(y1)==currenty)
		cont(x0,y0);
	else{
		move(x0,y0);
		cont(x1,y1);
	}
}

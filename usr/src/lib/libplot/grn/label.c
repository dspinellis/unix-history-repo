/*-
 * Copyright (c) 1980, 1986, 1993
 *	The Regents of the University of California.  All rights reserved.
 *
 * This module is believed to contain source code proprietary to AT&T.
 * Use and redistribution is subject to the Berkeley Software License
 * Agreement and your Software Agreement with AT&T (Western Electric).
 */

#ifndef lint
static char sccsid[] = "@(#)label.c	8.1 (Berkeley) 6/4/93";
#endif /* not lint */

#include "grnplot.h"

/*---------------------------------------------------------
 *	This routine places a label starting at the current
 *	position.
 *
 *	Results:	None.
 *
 *	Side Effects:
 *	The string indicated by s starting at (curx, cury).
 *	The current position is NOT updated.
 *---------------------------------------------------------
 */
label(s)
char *s;
{
	if (!ingrnfile) erase();
	endvector();
	printf("BOTLEFT\n");
	outcurxy();
	printf("*\n%d %d\n%d %s\n",FONTSTYLE,FONTSIZE,strlen(s)-1,s);
}

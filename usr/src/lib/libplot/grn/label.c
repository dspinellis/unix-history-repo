/*-
 * Copyright (c) 1980, 1986 The Regents of the University of California.
 * All rights reserved.
 *
 * %sccs.include.proprietary.c%
 */

#ifndef lint
static char sccsid[] = "@(#)label.c	6.2 (Berkeley) %G%";
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

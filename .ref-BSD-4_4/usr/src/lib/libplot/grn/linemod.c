/*-
 * Copyright (c) 1980, 1986, 1993
 *	The Regents of the University of California.  All rights reserved.
 *
 * This module is believed to contain source code proprietary to AT&T.
 * Use and redistribution is subject to the Berkeley Software License
 * Agreement and your Software Agreement with AT&T (Western Electric).
 */

#ifndef lint
static char sccsid[] = "@(#)linemod.c	8.1 (Berkeley) 6/4/93";
#endif /* not lint */

#include "grnplot.h"

/*---------------------------------------------------------
 *	Linemod sets the current line drawing style.
 *
 *	Results:	None.
 *
 *	Side Effects:
 *	The line style is set based on string s which
 *	must be one of "dotted", "solid", "longdashed", "shortdashed",
 *	or "dotdashed".  If s isn't recognized, then "solid" is used.
 *---------------------------------------------------------
 */
linemod(s)
char *s;
{
    endvector();
    if (strcmp(s, "dotted") == 0)
	linestyle = 1;
    else if (strcmp(s, "longdashed") == 0)
	linestyle = 4;
    else if (strcmp(s, "shortdashed") == 0)
	linestyle = 4;
    else if (strcmp(s, "dotdashed") == 0)
	linestyle = 2;
    else linestyle = DEFAULTLINE;
}

/*
 * Copyright (c) 1980, 1986 Regents of the University of California.
 * All rights reserved.  The Berkeley software License Agreement
 * specifies the terms and conditions for redistribution.
 */

#ifndef lint
static char sccsid[] = "@(#)linemod.c	6.1 (Berkeley) %G%";
#endif not lint


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

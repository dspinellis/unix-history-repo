/*-
 * Copyright (c) 1983, 1993
 *	The Regents of the University of California.  All rights reserved.
 *
 * %sccs.include.proprietary.c%
 */

#ifndef lint
static char sccsid[] = "@(#)linemod.c	8.1 (Berkeley) %G%";
#endif /* not lint */

#include "aed.h"

/*---------------------------------------------------------
 *	Linemod sets the current line drawing style.
 *
 *	Results:	None.
 *
 *	Side Effects:
 *	The AED line style is set based on string s which
 *	must be one of "dotted", "solid", "longdashed", "shortdashed",
 *	or "dotdashed".  If s isn't recognized, then "solid" is used.
 *---------------------------------------------------------
 */
linemod(s)
char *s;
{
    if (strcmp(s, "dotted") == 0)
	fputs("1AAFF", stdout);
    else if (strcmp(s, "longdashed") == 0)
	fputs("1F055", stdout);
    else if (strcmp(s, "shortdashed") == 0)
	fputs("1F0FF", stdout);
    else if (strcmp(s, "dotdashed") == 0)
	fputs("1E4FF", stdout);
    else fputs("1FFFF", stdout);
    (void) fflush(stdout);
}

#ifndef lint
static char sccsid[] = "@(#)linemod.c	4.1 (Berkeley) 11/11/83";
#endif

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

#ifndef lint
static char sccsid[] = "@(#)label.c	4.1 (Berkeley) 11/11/83";
#endif

#include "aed.h"

/*---------------------------------------------------------
 *	This routine places a label starting at the current
 *	position.
 *
 *	Results:	None.
 *
 *	Side Effects:
 *	The string indicated by s starting at (curx, cury).
 *	The current position is updated accordingly.
 *---------------------------------------------------------
 */
label(s)
char *s;
{
    setcolor("02");
    putc('Q', stdout);
    outxy20(curx + (4096/scale), cury + (4096/scale));
    putc('\6', stdout);
    fputs(s, stdout);
    putc('\33', stdout);
    (void) fflush(stdout);
    curx += ((6*4096*strlen(s)) + 4000)/scale;
}

#ifndef lint
static char sccsid[] = "@(#)move.c	4.1 (Berkeley) 11/11/83";
#endif

#include "aed.h"

/*---------------------------------------------------------
 *	This routine moves the current point to (x,y).
 *
 *	Results:	None.
 *	Side Effects:	As above.
 *---------------------------------------------------------
 */
move(x, y)
int x, y;
{
    curx = x;
    cury = y;
}

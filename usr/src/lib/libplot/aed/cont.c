#ifndef lint
static char sccsid[] = "@(#)cont.c	4.1 (Berkeley) %G%";
#endif

#include "aed.h"

/*---------------------------------------------------------
 *	Cont plots a line between (curx, cury) and (x, y).
 *
 *	Results:	None.
 *	Side Effects:	As above.
 *---------------------------------------------------------
 */
cont(x, y)
int x, y;
{
    line(curx, cury, x, y);
}

/*	move.c	4.1	83/03/09	*/
/*
 * move to (x, y).  Both the _pen and cursor are supposed to be moved.
 * We really just remember it for later, in case we move again.
 */

#include "2648.h"

move(x, y)
{
#ifdef TRACE
	if (trace)
		fprintf(trace, "\tmove(%d, %d), ", x, y);
#endif
	_supx = x;
	_supy = y;
}

/*
 * draw a line from the current place to (x,y).  Such lines are
 * supposed to be horizontal, and are affected by the current mode.
 */
#include "2648.h"

draw(x, y)
{
#ifdef TRACE
	if (trace) {
		fprintf(trace, "draw(%d,%d)\n", x, y);
	}
#endif
	sync();
	escseq(ESCP);
	motion(x, y);
	_supx = x;
	_supy = y;
}

#ifndef lint
static char sccsid[] = "@(#)space.c	4.1 (Berkeley) %G%";
#endif

#include "dumb.h"

space(x0, y0, x1, y1)
	int x0, y0, x1, y1;
{
	minX = x0;
	rangeX = x1 -x0;
	minY = y0;
	rangeY = y1 - y0;
}

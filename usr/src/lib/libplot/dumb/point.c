#ifndef lint
static char sccsid[] = "@(#)point.c	4.1 (Berkeley) %G%";
#endif

#include "dumb.h"

point(x, y)
	int x,y;
{
	scale(x, y);
	currentx = x;
	currenty = y;
	screenmat[currentx][currenty] = '*';
}

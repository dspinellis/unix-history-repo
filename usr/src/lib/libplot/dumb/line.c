#ifndef lint
static char sccsid[] = "@(#)line.c	4.1 (Berkeley) %G%";
#endif

#include "dumb.h"

line(x0, y0, x1, y1)
	int x0, y0, x1, y1;
{
	int x,y;

	scale(x0, y0);
	x = x1;
	y = y1;
	scale(x, y);
	currentx = x0;
	currenty = y0;
	screenmat[currentx][currenty] = '*';
	dda_line('*', x0, y0, x1, y1);
}

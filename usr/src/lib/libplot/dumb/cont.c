#ifndef lint
static char sccsid[] = "@(#)cont.c	4.1 (Berkeley) %G%";
#endif

#include "dumb.h"

cont(x, y)
	int x,y;
{
	int x1, y1;
	x1 = x;
	y1 = y;
	scale(x1, y1);
	dda_line('*', currentx, currenty, x, y);
}

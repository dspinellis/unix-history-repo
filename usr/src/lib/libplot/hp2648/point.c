#ifndef lint
static char sccsid[] = "@(#)point.c	4.1 (Berkeley) %G%";
#endif

#include "hp2648.h"

point(xi,yi)
int xi,yi;
{
	if(xsc(xi)!=currentx || ysc(yi)!=currenty)
		move(xi,yi);
	buffready(1);
	putchar('d');
}

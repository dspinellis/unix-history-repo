#ifndef lint
static char sccsid[] = "@(#)point.c	4.1 (Berkeley) %G%";
#endif

#include "gigi.h"

point(xi,yi)
int xi,yi;
{
	if(xsc(xi)!=currentx || ysc(yi)!=currenty)
		move(xi,yi);
	printf("V[]");
}

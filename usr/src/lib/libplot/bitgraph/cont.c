#ifndef lint
static char sccsid[] = "@(#)cont.c	4.1 (Berkeley) %G%";
#endif

#include "bg.h"

cont(xi,yi)
int xi,yi;
{
	currentx = scaleX(xi);
	currenty = scaleY(yi);
	putchar( ESC );
	printf(":%d;%dd", currentx, currenty);
}

#ifndef lint
static char sccsid[] = "@(#)circle.c	4.1 (Berkeley) %G%";
#endif

#include "gigi.h"

circle (xc,yc,r)
int xc,yc,r;
{
	if(r < 1){
		point(xc, yc);
		return;
	}
	move(xc, yc);
	printf("C[%d]", r);
}

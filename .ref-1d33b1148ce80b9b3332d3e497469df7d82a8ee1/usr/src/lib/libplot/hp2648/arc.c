/*-
 * Copyright (c) 1980, 1993
 *	The Regents of the University of California.  All rights reserved.
 *
 * %sccs.include.proprietary.c%
 */

#ifndef lint
static char sccsid[] = "@(#)arc.c	8.1 (Berkeley) %G%";
#endif /* not lint */

#include "hp2648.h"

arc(xcent,ycent,xbeg,ybeg,xend,yend)
int xcent,ycent,xbeg,ybeg,xend,yend;
{
	double costheta,sintheta,x,y,xn,r;
	double x1,y1,x2,y2;
	int xi,yi,crosspflag,crossp;

	r = (xcent-xbeg)*(xcent-xbeg)+(ycent-ybeg)*(ycent-ybeg);
	r = pow(r,0.5);
	if(r<1){
		point(xcent,ycent);
		return;
	}
	sintheta = 1.0/r;
	costheta = pow(1-sintheta*sintheta,0.5);
	xi = x = xbeg-xcent;
	yi = y = ybeg-ycent;
	x1 = xcent;
	y1 = ycent;
	x2 = xend;
	y2 = yend;
	crosspflag = 0;
	do {
		crossp = cross_product(x1,y1,x2,y2,x,y);
		if(crossp >0 && crosspflag == 0) crosspflag = 1;
		point(xcent+xi,ycent+yi);
		xn = x;
		xi = x = x*costheta + y*sintheta;
		yi = y = y*costheta - xn*sintheta;
	} while( crosspflag == 0 || crossp >0);
}

cross_product(x1,y1,x2,y2,x3,y3)
double x1,x2,x3,y1,y2,y3;
{
	double z,a,b;
	a = (y3-y2)*(x2-x1);
	b = (x3-x2)*(y2-y1);
	z = a-b;
	if(z<0) return(-1);
	if(z>0) return(1);
	return(0);
}

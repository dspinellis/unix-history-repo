/*-
 * Copyright (c) 1980, 1993
 *	The Regents of the University of California.  All rights reserved.
 *
 * This module is believed to contain source code proprietary to AT&T.
 * Use and redistribution is subject to the Berkeley Software License
 * Agreement and your Software Agreement with AT&T (Western Electric).
 */

#ifndef lint
static char sccsid[] = "@(#)circle.c	8.1 (Berkeley) 6/4/93";
#endif /* not lint */

#include "hp2648.h"

circle (xc,yc,r)
int xc,yc,r;
{
	double costheta,sintheta,x,y,xn;
	int xi,yi;

	if(r<1){
		point(xc,yc);
		return;
	}
	sintheta = 1.0/r;
	costheta = pow(1-sintheta*sintheta,0.5);
	xi = x = r;
	yi = y = 0;
	do {
		point(xc+xi,yc+yi);
		xn = x;
		xi = x = x*costheta + y*sintheta;
		yi = y = y*costheta - xn*sintheta;
	} while( ! (yi==0 && xi >= r-1));
}

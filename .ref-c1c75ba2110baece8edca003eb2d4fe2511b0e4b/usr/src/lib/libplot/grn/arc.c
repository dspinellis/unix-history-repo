/*
 * Copyright (c) 1980, 1986 Regents of the University of California.
 * All rights reserved.  The Berkeley software License Agreement
 * specifies the terms and conditions for redistribution.
 */

#ifndef lint
static char sccsid[] = "@(#)arc.c	6.1 (Berkeley) %G%";
#endif not lint

#include "grnplot.h"


arc(x,y,x0,y0,x1,y1)
{
	extern double atan2();

	if (!ingrnfile) erase();
	endvector();
	printf("ARC\n");
	outxy(x,y);
	outxy(x0,y0);
	outxy(x1,y1);
	printf("*\n%d %d\n0\n",linestyle,(int) (atan2(x1-x,y1-y)-atan2(x0-x,y0-y)));
	curx =x;
	cury =y;
}

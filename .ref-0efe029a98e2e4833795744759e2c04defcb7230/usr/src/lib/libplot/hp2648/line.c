/*
 * Copyright (c) 1980 Regents of the University of California.
 * All rights reserved.  The Berkeley software License Agreement
 * specifies the terms and conditions for redistribution.
 */

#ifndef lint
static char sccsid[] = "@(#)line.c	5.1 (Berkeley) %G%";
#endif not lint

#include "hp2648.h"

line(x0,y0,x1,y1)
int x0,y0,x1,y1;
{
	if(xsc(x0)==currentx && ysc(y0)==currenty)
		cont(x1,y1);
	else if(xsc(x1)==currentx && ysc(y1)==currenty)
		cont(x0,y0);
	else{
		move(x0,y0);
		cont(x1,y1);
	}
}

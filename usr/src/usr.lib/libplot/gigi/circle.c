/*
 * Copyright (c) 1980 Regents of the University of California.
 * All rights reserved.  The Berkeley software License Agreement
 * specifies the terms and conditions for redistribution.
 */

#ifndef lint
static char sccsid[] = "@(#)circle.c	5.1 (Berkeley) 5/7/85";
#endif not lint

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

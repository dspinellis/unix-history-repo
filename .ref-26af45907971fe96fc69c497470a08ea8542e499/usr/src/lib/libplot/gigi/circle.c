/*-
 * Copyright (c) 1980, 1993
 *	The Regents of the University of California.  All rights reserved.
 *
 * %sccs.include.proprietary.c%
 */

#ifndef lint
static char sccsid[] = "@(#)circle.c	8.1 (Berkeley) %G%";
#endif /* not lint */

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

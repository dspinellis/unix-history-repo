/*-
 * Copyright (c) 1980 The Regents of the University of California.
 * All rights reserved.
 *
 * %sccs.include.proprietary.c%
 */

#ifndef lint
static char sccsid[] = "@(#)line.c	5.3 (Berkeley) %G%";
#endif /* not lint */

#include "bg.h"

line(x0,y0,x1,y1)
int x0,y0,x1,y1;
{
	if(scaleX(x0)==currentx && scaleY(y0)==currenty)
		cont(x1,y1);
	else if(scaleX(x1)==currentx && scaleY(y1)==currenty)
		cont(x0,y0);
	else{
		move(x0,y0);
		cont(x1,y1);
	}
}

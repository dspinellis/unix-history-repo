/*-
 * Copyright (c) 1980, 1993
 *	The Regents of the University of California.  All rights reserved.
 *
 * %sccs.include.proprietary.c%
 */

#ifndef lint
static char sccsid[] = "@(#)point.c	8.1 (Berkeley) %G%";
#endif /* not lint */

#include "hp7221.h"

point(xi,yi)
int xi,yi;
{
	if(scaleX(xi)!=currentx || scaleY(yi)!=currenty)
		move(xi,yi);
	cont(xi, yi);
}

/*-
 * Copyright (c) 1980 The Regents of the University of California.
 * All rights reserved.
 *
 * %sccs.include.proprietary.c%
 */

#ifndef lint
static char sccsid[] = "@(#)space.c	5.2 (Berkeley) %G%";
#endif /* not lint */

#include "gigi.h"

space(x0,y0,x1,y1)
int x0,y0,x1,y1;
{
	lowx = x0;
	lowy = y0;
	scalex = XMAX/(x1-lowx);
	scaley = YMAX/(y1-lowy);
}

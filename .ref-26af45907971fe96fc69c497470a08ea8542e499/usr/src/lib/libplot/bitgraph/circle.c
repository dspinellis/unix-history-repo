/*-
 * Copyright (c) 1980, 1993
 *	The Regents of the University of California.  All rights reserved.
 *
 * %sccs.include.proprietary.c%
 */

#ifndef lint
static char sccsid[] = "@(#)circle.c	8.1 (Berkeley) %G%";
#endif /* not lint */

circle (xc,yc,r)
int xc,yc,r;
{
	arc(xc,yc, xc+r,yc, xc-r,yc);
	arc(xc,yc, xc-r,yc, xc+r,yc);
}

/*-
 * Copyright (c) 1980 The Regents of the University of California.
 * All rights reserved.
 *
 * %sccs.include.proprietary.c%
 */

#ifndef lint
static char sccsid[] = "@(#)point.c	5.3 (Berkeley) %G%";
#endif /* not lint */

point(xi, yi)
int xi, yi;
{
	move(xi, yi);
	label(".");
}

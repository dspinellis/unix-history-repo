/*-
 * Copyright (c) 1980 The Regents of the University of California.
 * All rights reserved.
 *
 * %sccs.include.proprietary.c%
 */

#ifndef lint
static char sccsid[] = "@(#)line.c	5.2 (Berkeley) %G%";
#endif /* not lint */

line_(x0,y0,x1,y1)
int *x0, *y0, *x1, *y1;
{
	line(*x0,*y0,*x1,*y1);
}

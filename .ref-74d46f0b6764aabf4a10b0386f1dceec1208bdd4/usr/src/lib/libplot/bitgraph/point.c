/*
 * Copyright (c) 1980 Regents of the University of California.
 * All rights reserved.  The Berkeley software License Agreement
 * specifies the terms and conditions for redistribution.
 */

#ifndef lint
static char sccsid[] = "@(#)point.c	5.2 (Berkeley) %G%";
#endif not lint


point(xi, yi)
int xi, yi;
{
	move(xi, yi);
	label(".");
}

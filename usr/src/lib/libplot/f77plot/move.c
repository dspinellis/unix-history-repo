/*
 * Copyright (c) 1980 Regents of the University of California.
 * All rights reserved.  The Berkeley software License Agreement
 * specifies the terms and conditions for redistribution.
 */

#ifndef lint
static char sccsid[] = "@(#)move.c	5.1 (Berkeley) %G%";
#endif not lint

move_(xi,yi)
int *xi, *yi;
{
	move(*xi,*yi);
}

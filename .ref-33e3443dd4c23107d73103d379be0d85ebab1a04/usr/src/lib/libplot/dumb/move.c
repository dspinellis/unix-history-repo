/*
 * Copyright (c) 1980 Regents of the University of California.
 * All rights reserved.  The Berkeley software License Agreement
 * specifies the terms and conditions for redistribution.
 */

#ifndef lint
static char sccsid[] = "@(#)move.c	5.1 (Berkeley) %G%";
#endif not lint

#include "dumb.h"

move(x,y)
int x,y;
{
	scale(x, y);
	currentx = x;
	currenty = y;
}

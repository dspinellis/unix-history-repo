/*
 * Copyright (c) 1980 Regents of the University of California.
 * All rights reserved.  The Berkeley software License Agreement
 * specifies the terms and conditions for redistribution.
 */

#ifndef lint
static char sccsid[] = "@(#)agoto.c	5.1 (Berkeley) 4/26/85";
#endif not lint

/*
 * position the alphanumeric cursor to (x, y).
 */

#include "2648.h"

agoto(x, y)
int x, y;
{
	char mes[20];
	sprintf(mes, "\33*dE\33&a%dr%dC", x, y);
	outstr(mes);
}

/*
 * lower left corner of screen.
 */
lowleft()
{
	outstr("\33F");
}

/*
 * Copyright (c) 1980 Regents of the University of California.
 * All rights reserved.  The Berkeley software License Agreement
 * specifies the terms and conditions for redistribution.
 */

#ifndef lint
static char sccsid[] = "@(#)cont.c	5.1 (Berkeley) 5/7/85";
#endif not lint

#include "hp7221.h"

cont(xi,yi)
int xi,yi;
{
	currentx = scaleX(xi);
	currenty = scaleY(yi);
	putchar( 'q' );
	putMBP( currentx, currenty );
}

/*-
 * Copyright (c) 1980 The Regents of the University of California.
 * All rights reserved.
 *
 * %sccs.include.proprietary.c%
 */

#ifndef lint
static char sccsid[] = "@(#)move.c	5.2 (Berkeley) %G%";
#endif /* not lint */

#include "hp7221.h"

move(xi,yi)
int xi,yi;
{
	currentx = scaleX(xi);
	currenty = scaleY(yi);
	putchar( 'p' );
	putMBP( currentx, currenty );
}

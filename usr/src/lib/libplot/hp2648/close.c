/*-
 * Copyright (c) 1980 The Regents of the University of California.
 * All rights reserved.
 *
 * %sccs.include.proprietary.c%
 */

#ifndef lint
static char sccsid[] = "@(#)close.c	5.2 (Berkeley) %G%";
#endif /* not lint */

#include "hp2648.h"

closepl()
{
	putchar('Z');
	fflush(stdout);
	if ( shakehands == TRUE ) {
		stty(fildes, &sarg);
		close(fildes);
	}
}

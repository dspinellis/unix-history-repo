/*-
 * Copyright (c) 1980, 1993
 *	The Regents of the University of California.  All rights reserved.
 *
 * %sccs.include.proprietary.c%
 */

#ifndef lint
static char sccsid[] = "@(#)erase.c	8.1 (Berkeley) %G%";
#endif /* not lint */

#include "bg.h"

erase()
{
	putchar( ESC );
	printf("[H");
	putchar( ESC );
	printf("[J");
}

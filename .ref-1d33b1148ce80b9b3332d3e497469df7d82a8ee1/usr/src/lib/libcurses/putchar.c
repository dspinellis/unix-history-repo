/*
 * Copyright (c) 1981, 1993, 1994
 *	The Regents of the University of California.  All rights reserved.
 *
 * %sccs.include.redist.c%
 */

#ifndef lint
static char sccsid[] = "@(#)putchar.c	8.2 (Berkeley) %G%";
#endif	/* not lint */

#include "curses.h"

void
__cputchar(ch)
	int ch;
{

#ifdef DEBUG
	__CTRACE("__cputchar: %s\n", unctrl(ch));
#endif
	(void)putchar(ch);
}

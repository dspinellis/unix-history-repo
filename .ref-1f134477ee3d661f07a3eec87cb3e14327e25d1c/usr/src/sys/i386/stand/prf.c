/*-
 * Copyright (c) 1990, 1993
 *	The Regents of the University of California.  All rights reserved.
 *
 * This code is derived from software contributed to Berkeley by
 * William Jolitz.
 *
 * %sccs.include.redist.c%
 *
 *	@(#)prf.c	8.1 (Berkeley) %G%
 */

#include <sys/types.h>

putchar(c)
char c;
{
        if (c == '\n')
		sput('\r');
	sput(c);
	return(0);
}

wait(n) { while(n--) ; }

/*-
 * Copyright (c) 1980 The Regents of the University of California.
 * All rights reserved.
 *
 * %sccs.include.proprietary.c%
 */

#ifndef lint
static char sccsid[] = "@(#)long_.c	5.2 (Berkeley) %G%";
#endif /* not lint */

/*
 * convert short ints to long.
 * Needed for literals in -I2 compiles.
 * used as follows:
 *	integer*4 long
 *	...
 *	call ftell(long(11))
 */

long long_(i)
short *i;
{	return((long)*i);	}

/*-
 * Copyright (c) 1980 The Regents of the University of California.
 * All rights reserved.
 *
 * %sccs.include.proprietary.c%
 */

#ifndef lint
static char sccsid[] = "@(#)short_.c	5.2 (Berkeley) %G%";
#endif /* not lint */

/*
 * convert long ints to short.
 *
 * used as follows:
 *	integer*2 short
 *	...
 *	call mysub(short(ivar))
 * where:
 *	mysub expects to receive an integer*2 arg and ivar is integer*4
 */

short short_(i)
long *i;
{	return((short)*i);	}

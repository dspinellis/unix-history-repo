/*
 * Copyright (c) 1980 Regents of the University of California.
 * All rights reserved.  The Berkeley software License Agreement
 * specifies the terms and conditions for redistribution.
 *
 *	@(#)short_.c	5.1	%G%
 */

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

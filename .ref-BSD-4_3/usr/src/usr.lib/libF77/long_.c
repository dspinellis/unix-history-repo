/*
 * Copyright (c) 1980 Regents of the University of California.
 * All rights reserved.  The Berkeley software License Agreement
 * specifies the terms and conditions for redistribution.
 *
 *	@(#)long_.c	5.1	6/7/85
 */

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

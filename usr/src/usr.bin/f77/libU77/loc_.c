/*
 * Copyright (c) 1980 Regents of the University of California.
 * All rights reserved.  The Berkeley software License Agreement
 * specifies the terms and conditions for redistribution.
 *
 *	@(#)loc_.c	5.1	%G%
 */

/*
 * Return the address of the argument.
 *
 * calling sequence:
 *	iloc = loc (arg)
 * where:
 *	iloc will receive the address of arg
 */

long loc_(arg)
long *arg;
{
	return((long)arg);
}

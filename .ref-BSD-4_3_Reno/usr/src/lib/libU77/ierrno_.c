/*
 * Copyright (c) 1980 Regents of the University of California.
 * All rights reserved.  The Berkeley software License Agreement
 * specifies the terms and conditions for redistribution.
 *
 *	@(#)ierrno_.c	5.1	6/7/85
 */

/*
 * return the current value of the system error register
 *
 * calling sequence:
 *	ier = ierrno()
 * where:
 *	ier will receive the current value of errno
 */

extern int errno;

long ierrno_()
{
	return((long)errno);
}

/*-
 * Copyright (c) 1980 The Regents of the University of California.
 * All rights reserved.
 *
 * %sccs.include.proprietary.c%
 */

#ifndef lint
static char sccsid[] = "@(#)ierrno_.c	5.2 (Berkeley) %G%";
#endif /* not lint */

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

/*-
 * Copyright (c) 1980 The Regents of the University of California.
 * All rights reserved.
 *
 * %sccs.include.proprietary.c%
 */

#ifndef lint
static char sccsid[] = "@(#)sleep_.c	5.2 (Berkeley) %G%";
#endif /* not lint */

/*
 * sleep for awhile
 *
 * calling sequence:
 *	call sleep(seconds)
 * where:
 *	seconds is an integer number of seconds to sleep (see sleep(3))
 */

sleep_(sec)
long *sec;
{
	sleep((unsigned int)*sec);
}

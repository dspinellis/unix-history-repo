/*-
 * Copyright (c) 1980 The Regents of the University of California.
 * All rights reserved.
 *
 * %sccs.include.proprietary.c%
 */

#ifndef lint
static char sccsid[] = "@(#)itime_.c	5.2 (Berkeley) %G%";
#endif /* not lint */

/*
 * return the current time in numerical form
 *
 * calling sequence:
 *	integer iarray(3)
 *	call itime(iarray)
 * where:
 *	iarray will receive the current time; hour, min, sec.
 */

#include <sys/types.h>
#include <sys/time.h>

itime_(iar)
struct { long ihr; long imin; long isec; } *iar;
{
	struct tm *localtime(), *lclt;
	long int time(), t;

	t = time(0);
	lclt = localtime(&t);
	iar->ihr = lclt->tm_hour;
	iar->imin = lclt->tm_min;
	iar->isec = lclt->tm_sec;
}

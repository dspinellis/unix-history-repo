/*-
 * Copyright (c) 1980 The Regents of the University of California.
 * All rights reserved.
 *
 * %sccs.include.proprietary.c%
 */

#ifndef lint
static char sccsid[] = "@(#)idate_.c	5.2 (Berkeley) %G%";
#endif /* not lint */

/*
 * return date in numerical form
 *
 * calling sequence:
 *	integer iarray(3)
 *	call idate(iarray)
 * where:
 *	iarray will receive the current date; day, mon, year.
 */

#include <sys/types.h>
#include <sys/time.h>

idate_(iar)
struct { long iday; long imon; long iyer; } *iar;
{
	struct tm *localtime(), *lclt;
	long int time(), t;

	t = time(0);
	lclt = localtime(&t);
	iar->iday = lclt->tm_mday;
	iar->imon = lclt->tm_mon + 1;
	iar->iyer = lclt->tm_year + 1900;
}

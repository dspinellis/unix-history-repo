/*
 * Copyright (c) 1980 Regents of the University of California.
 * All rights reserved.  The Berkeley software License Agreement
 * specifies the terms and conditions for redistribution.
 *
 *	@(#)idate_.c	5.1	6/7/85
 */

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

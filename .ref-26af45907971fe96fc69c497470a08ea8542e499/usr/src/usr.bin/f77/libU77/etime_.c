/*-
 * Copyright (c) 1980 The Regents of the University of California.
 * All rights reserved.
 *
 * %sccs.include.proprietary.c%
 */

#ifndef lint
static char sccsid[] = "@(#)etime_.c	5.2 (Berkeley) %G%";
#endif /* not lint */

/*
 * Return the elapsed execution time for this process.
 *
 * calling sequence:
 * 	real time(2)
 * 	call etime (time)
 * where:
 * 	the 2 element array, time, will receive the user and system
 * 	elapsed time since the start of execution.
 *
 *	This routine can be called as function, and returns the sum of
 *	user and system times. The time array argument must always be given.
 *
 *	The resolution for all timing is 1/60 second.
 */

#include <sys/types.h>
#include <sys/times.h>

struct tb { float usrtime; float systime; };

float
etime_(et) struct tb *et;
{	struct tms clock;

	times(&clock);
	et->usrtime = (float) clock.tms_utime / 60.0;
	et->systime = (float) clock.tms_stime / 60.0;
	return(et->usrtime + et->systime);
}

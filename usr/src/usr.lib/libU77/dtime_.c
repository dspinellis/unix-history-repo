/*
 * Copyright (c) 1980 Regents of the University of California.
 * All rights reserved.  The Berkeley software License Agreement
 * specifies the terms and conditions for redistribution.
 *
 *	@(#)dtime_.c	5.1	6/7/85
 */

/*
 * Returns the delta time since the last call to dtime.
 *
 * calling sequence:
 * 	real time(2)
 * 	call dtime(time)
 * where:
 * 	the 2 element array time will receive the user and system
 * 	elapsed time since the last call to dtime, or since the start
 * 	of execution.
 *
 * This routine can be called as function, and returns the sum of
 * user and system times. The time_array argument must always be given.
 *
 * The resolution for all timing is 1/60 second.
 */

#include <sys/types.h>
#include <sys/times.h>

struct tb { float usrtime; float systime; };

time_t dutime=0, dstime=0;

float
dtime_(dt) struct tb *dt;
{	struct tms clock;

	times(&clock);
	dt->usrtime = (float)(clock.tms_utime - dutime) / 60.0;
	dt->systime = (float)(clock.tms_stime - dstime) / 60.0;
	dutime = clock.tms_utime;
	dstime = clock.tms_stime;
	return(dt->usrtime + dt->systime);
}

/*
 * Copyright (c) 1980 Regents of the University of California.
 * All rights reserved.  The Berkeley software License Agreement
 * specifies the terms and conditions for redistribution.
 */

#if defined(LIBC_SCCS) && !defined(lint)
static char sccsid[] = "@(#)ftime.c	5.2 (Berkeley) 3/9/86";
#endif LIBC_SCCS and not lint

#include <sys/types.h>
#include <sys/time.h>

/*
 * Backwards compatible ftime.
 */

/* from old timeb.h */
struct timeb {
	time_t	time;
	u_short	millitm;
	short	timezone;
	short	dstflag;
};

ftime(tp)
	register struct timeb *tp;
{
	struct timeval t;
	struct timezone tz;

	if (gettimeofday(&t, &tz) < 0)
		return (-1);
	tp->time = t.tv_sec;
	tp->millitm = t.tv_usec / 1000;
	tp->timezone = tz.tz_minuteswest;
	tp->dstflag = tz.tz_dsttime;
}

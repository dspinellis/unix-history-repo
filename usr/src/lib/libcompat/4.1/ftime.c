/*-
 * Copyright (c) 1980, 1993
 *	The Regents of the University of California.  All rights reserved.
 *
 * This module is believed to contain source code proprietary to AT&T.
 * Use and redistribution is subject to the Berkeley Software License
 * Agreement and your Software Agreement with AT&T (Western Electric).
 */

#if defined(LIBC_SCCS) && !defined(lint)
static char sccsid[] = "@(#)ftime.c	8.1 (Berkeley) 6/4/93";
#endif /* LIBC_SCCS and not lint */

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
	struct tm *tm;
	time_t zero;

	if (gettimeofday(&t, &tz) < 0)
		return (-1);
	tp->time = t.tv_sec;
	tp->millitm = t.tv_usec / 1000;
	if ((tm = localtime(&tp->time)) == NULL) {
		/* in the absence of anything better, use kernel's timezone */
		tp->timezone = tz.tz_minuteswest;
		tp->dstflag = tz.tz_dsttime;
	} else {
		tp->dstflag = tm->tm_isdst;
		if (tm->tm_isdst) {	/* tm_gmtoff has an offset applied */
			zero = 0;	/* try 0 and hope for the best */
			tp->timezone = -localtime(&zero)->tm_gmtoff / 60;
		} else
			tp->timezone = -tm->tm_gmtoff / 60;
	}
}

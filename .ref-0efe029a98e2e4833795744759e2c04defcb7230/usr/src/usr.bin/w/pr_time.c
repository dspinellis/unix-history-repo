/*-
 * Copyright (c) 1990 The Regents of the University of California.
 * All rights reserved.
 *
 * %sccs.include.redist.c%
 */

#ifndef lint
static char sccsid[] = "@(#)pr_time.c	5.2 (Berkeley) %G%";
#endif /* not lint */

#include <sys/types.h>
#include <sys/time.h>
#include <stdio.h>
#include <string.h>

#define	HR	(60 * 60)
#define	DAY	(24 * HR)
#define	MON	(30 * DAY)

static time_t now;
/*
 * prttime prints a time in hours and minutes or minutes and seconds.
 * The character string tail is printed at the end, obvious
 * strings to pass are "", " ", or "am".
 */
static char *
prttime(tim, tail)
	time_t tim;
	char *tail;
{
	int mins;
	static char timebuf[32];

	if (tim >= 60) {
		mins = tim % 60;
		(void) sprintf(timebuf, "%2d:%02d%s", (int)(tim / 60), mins,
		    tail);
	} else if (tim >= 0)
		(void) sprintf(timebuf, "    %2d%s", (int)tim, tail);
	else
		(void) strcpy(timebuf, tail);

	return (timebuf);
}

static char *weekday[] = { "Sun", "Mon", "Tue", "Wed", "Thu", "Fri", "Sat" };
static char *month[] = { "Jan", "Feb", "Mar", "Apr", "May", "Jun",
		"Jul", "Aug", "Sep", "Oct", "Nov", "Dec" };

/* prtat prints a 12 hour time given a pointer to a time of day */
char *
attime(started)
	time_t *started;
{
	struct tm *p;
	register int hr, pm;
	static char prbuff[64];

	if (now == 0)
		(void) time(&now);
	p = localtime(started);
	hr = p->tm_hour;
	pm = (hr > 11);
	if (hr > 11)
		hr -= 12;
	if (hr == 0)
		hr = 12;
	if (now - *started <= 18 * HR)
		return (prttime((time_t)hr * 60 + p->tm_min, pm ? "pm" : "am"));
	if (now - *started <= 7 * DAY)
		(void) sprintf(prbuff, "%*s%d%s", hr < 10 ? 4 : 3,
			weekday[p->tm_wday], hr, pm ? "pm" : "am");
	else
		(void) sprintf(prbuff, "%2d%s%2d", p->tm_mday,
			month[p->tm_mon], p->tm_year);

	return (prbuff);
}

/*-
 * Copyright (c) 1990 The Regents of the University of California.
 * All rights reserved.
 *
 * Redistribution and use in source and binary forms are permitted
 * provided that: (1) source distributions retain this entire copyright
 * notice and comment, and (2) distributions including binaries display
 * the following acknowledgement:  ``This product includes software
 * developed by the University of California, Berkeley and its contributors''
 * in the documentation or other materials provided with the distribution
 * and in all advertising materials mentioning features or use of this
 * software. Neither the name of the University nor the names of its
 * contributors may be used to endorse or promote products derived
 * from this software without specific prior written permission.
 * THIS SOFTWARE IS PROVIDED ``AS IS'' AND WITHOUT ANY EXPRESS OR
 * IMPLIED WARRANTIES, INCLUDING, WITHOUT LIMITATION, THE IMPLIED
 * WARRANTIES OF MERCHANTABILITY AND FITNESS FOR A PARTICULAR PURPOSE.
 */

#ifndef lint
static char sccsid[] = "@(#)attime.c	5.1 (Berkeley) 5/4/90";
#endif /* not lint */

#include <sys/types.h>
#include <sys/time.h>
#include <stdio.h>

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
	static char timebuf[32], *cp;

	if (tim >= 60) {
		int mins = tim % 60;
		sprintf(timebuf, "%2d:%02d", tim/60, mins);
	} else if (tim >= 0)
		sprintf(timebuf, "    %2d", tim);
	cp = timebuf;
	while (*cp)
		cp++;
	sprintf(cp, "%s", tail);

	return (timebuf);
}

static char *weekday[] = { "Sun", "Mon", "Tue", "Wed", "Thu", "Fri", "Sat" };
static char *month[] = { "Jan", "Feb", "Mar", "Apr", "May", "Jun",
		"Jul", "Aug", "Sep", "Oct", "Nov", "Dec" };

/* prtat prints a 12 hour time given a pointer to a time of day */
char *
attime(started)
	long *started;
{
	struct tm *p;
	register int hr, pm;
	static char prbuff[64];

	if (now == 0)
		time(&now);
	p = localtime(started);
	hr = p->tm_hour;
	pm = (hr > 11);
	if (hr > 11)
		hr -= 12;
	if (hr == 0)
		hr = 12;
	if (now - *started <= 18 * HR)
		return (prttime(hr * 60 + p->tm_min, pm ? "pm" : "am"));
	else if (now - *started <= 7 * DAY)
		sprintf(prbuff, "%*s%d%s", hr < 10 ? 4 : 3,
			weekday[p->tm_wday], hr, pm ? "pm" : "am");
	else
		sprintf(prbuff, "%2d%s%2d", p->tm_mday, month[p->tm_mon], 
			p->tm_year);

	return (prbuff);
}

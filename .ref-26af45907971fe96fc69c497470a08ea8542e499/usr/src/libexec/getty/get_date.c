/*
 * Copyright (c) 1985 The Regents of the University of California.
 * All rights reserved.
 *
 * %sccs.include.redist.c%
 */

#ifndef lint
static char sccsid[] = "@(#)get_date.c	5.4 (Berkeley) %G%";
#endif /* not lint */

#include <stdio.h>
#include <sys/time.h>

static char *days[] = {
	"Sun", "Mon", "Tues", "Wed", "Thurs", "Fri", "Sat"
};

static char *months[] = {
	"Jan", "Feb", "Mar", "Apr", "May", "June",
	"July", "Aug", "Sept", "Oct", "Nov", "Dec"
};

#define AM "am"
#define PM "pm"

get_date(datebuffer)
	char *datebuffer;
{
	struct tm *localtime(), *tmp;
	struct timeval tv;
	int realhour;
	char *zone;

	gettimeofday(&tv, 0);
	tmp = localtime(&tv.tv_sec);

	realhour = tmp->tm_hour;
	zone = AM;			/* default to morning */
	if (tmp->tm_hour == 0)
		realhour = 12;		/* midnight */
	else if (tmp->tm_hour == 12)
		zone = PM;		/* noon */
	else if (tmp->tm_hour >= 13 && tmp->tm_hour <= 23) { /* afternoon */
		realhour = realhour - 12;
		zone = PM;
	}
	
	/* format is '8:10pm on Sunday, 16 Sept 1973' */

	(void)sprintf(datebuffer, "%d:%02d%s on %s, %d %s %d",
		realhour,
		tmp->tm_min,
		zone,
		days[tmp->tm_wday],
		tmp->tm_mday,
		months[tmp->tm_mon],
		1900 + tmp->tm_year);
}

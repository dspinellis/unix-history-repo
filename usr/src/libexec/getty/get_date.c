/*
 * Copyright (c) 1985 The Regents of the University of California.
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
static char sccsid[] = "@(#)get_date.c	5.4 (Berkeley) 6/1/90";
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

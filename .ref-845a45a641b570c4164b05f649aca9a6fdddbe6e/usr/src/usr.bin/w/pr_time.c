/*-
 * Copyright (c) 1990 The Regents of the University of California.
 * All rights reserved.
 *
 * %sccs.include.redist.c%
 */

#ifndef lint
static char sccsid[] = "@(#)pr_time.c	5.4 (Berkeley) %G%";
#endif /* not lint */

#include <sys/types.h>
#include <sys/time.h>

#include <stdio.h>
#include <string.h>
#include <tzfile.h>

#include "extern.h"

/*
 * pr_attime --
 *	Print the time since the user logged in. 
 *
 *	Note: SCCS forces the bizarre string manipulation, things like
 *	5.4 get replaced in the source code.
 */
void
pr_attime(started, now)
	time_t *started, *now;
{
	static char buf[256];
	struct tm *tp;
	time_t diff;
	char fmt[20];

	tp = localtime(started);
	diff = *now - *started;

	/* If more than a week, use day-month-year. */
	if (diff > SECSPERDAY * DAYSPERWEEK)
		(void)strcpy(fmt, "%d%b%y");

	/* If not today, use day-hour-am/pm. */
	else if (*now / SECSPERDAY != *started / SECSPERDAY) {
		(void)strcpy(fmt, "%a%%%p");
		fmt[3] = 'I';
	}

	/* Default is hh:mm{am,pm}. */
	else {
		(void)strcpy(fmt, "%l:%%%p");
		fmt[4] = 'M';
	}

	(void)strftime(buf, sizeof(buf), fmt, tp);
	(void)printf("%s", buf);
}

/*
 * pr_idle --
 *	Display the idle time.
 */
void
pr_idle(idle)
	time_t idle;
{
	/* If idle more than 36 hours, print as a number of days. */
	if (idle >= 36 * SECSPERHOUR)
		(void)printf(" %ddays ", idle / SECSPERDAY);

	/* If idle more than an hour, print as HH:MM. */
	else if (idle >= SECSPERHOUR)
		(void)printf(" %2d:%02d ",
		    idle / SECSPERHOUR, (idle % SECSPERHOUR) / SECSPERMIN);

	/* Else print the minutes idle. */
	else
		(void)printf("    %2d ", idle / SECSPERMIN);
}

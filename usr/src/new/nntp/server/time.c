#ifndef lint
static char	*sccsid = "@(#)time.c	1.7	(Berkeley) 6/26/87";
#endif

/*
 * Collection of routines for dealing with ASCII time strings.
 * These may actually be useful in their own right.
 */

#include <stdio.h>
#include <sys/types.h>
#include <sys/time.h>
#include <ctype.h>
#include <strings.h>

/*
 * dtol -- convert date to long integer.  This is not implicitly
 * local time, or any other kind of time, for that matter.  If you
 * pass it a date you think is GMT, you wind up with that number of
 * seconds...
 *
 *	Parameters:		"date_ascii" is in the form "yymmddhhmmss".
 *
 *	Returns:		Long integer containing number
 *				of seconds since 000000 Jan 1, 1970,
 *				and "date".  -1 on error.
 *
 *	Side effects:		None.
 */

#define twodigtoi(x)	(((x[0]) - '0') + (x[1] - '0')*10)
#define	dysize(y)	((y % 4 ? 365 : 366))

static	int	dmsize[12] =
    { 31, 28, 31, 30, 31, 30, 31, 31, 30, 31, 30, 31 };

long
dtol(date_ascii)
	char	*date_ascii;
{
	char	date[32], *date_str;
	char	*lhs, *rhs;
	char	temp;
	long	seconds;
	int	year, month, day, hour, mins, secs;
	int	len, i;

	len = strlen(date_ascii);
	if (len != sizeof("yymmddhhmmss")-1)
		return (-1);

	(void) strcpy(date, date_ascii);
	date_str = date;

#ifdef debug
	printf("date_str = %s\n", date_str);
#endif
	rhs = date_str + len - 1;
	lhs = date_str;

	for (; lhs < rhs; ++lhs, --rhs) {
		temp = *lhs;
		*lhs = *rhs;
		*rhs = temp;
		if (!isdigit(temp) || !isdigit(*lhs))
			return (-1);
	}

	lhs = date_str;
#ifdef debug
	printf("date_str = %s\n", date_str);
#endif

	secs = twodigtoi(lhs);
	lhs += 2;
	mins = twodigtoi(lhs);
	lhs += 2;
	hour = twodigtoi(lhs);
	lhs += 2;
	day = twodigtoi(lhs);
	lhs += 2;
	month = twodigtoi(lhs);
	lhs += 2;
	year = twodigtoi(lhs);

	if (month < 1 || month > 12 ||
	    day < 1 || day > 31 ||
	    mins < 0 || mins > 59 ||
	    secs < 0 || secs > 59)
		return (-1);
	if (hour == 24) {
		hour = 0;
		day++;
	}
	if (hour < 0 || hour > 23)
		return (-1);
	seconds = 0;
	year += 1900;
	for (i = 1970; i < year; i++)
		seconds += dysize(i);
	/* Leap year */
	if (dysize(year) == 366 && month >= 3)
		seconds++;
	while (--month)
		seconds += dmsize[month-1];
	seconds += day-1;
	seconds = 24 * seconds + hour;
	seconds = 60 * seconds + mins;
	seconds = 60 * seconds + secs;

	return (seconds);
}


/*
 * ltod -- convert long integer to date string.
 *
 *	Parameters:		"date" is in the number of seconds
 *				since the epoch.
 *
 *	Returns:		Pointer to static data in the form
 *				yymmddhhmmss\0.
 *
 *	Side effects:		None.
 */

char *
ltod(date)
	long		date;
{
	static char	timebuf[32];
	struct tm	*tp;

	tp = gmtime(&date);

	(void) sprintf(timebuf, "%02d%02d%02d%02d%02d%02d",
		tp->tm_year,
		tp->tm_mon + 1,		/* 0 to 11??? How silly. */
		tp->tm_mday,
		tp->tm_hour,
		tp->tm_min,
		tp->tm_sec);

	return (timebuf);
}


/*
 * local_to_gmt -- convert a local time (in form of number of
 * seconds since you-know-when) to GMT.
 *
 *	Parameters:	"date" is date we want converted, in
 *			seconds since 000000 1 Jan 1970.
 *
 *	Returns:	Number of seconds corrected for local
 *			and dst.
 */

long
local_to_gmt(date)
	long	date;
{
	struct	timeval	tv;
	struct	timezone tz;

	(void) gettimeofday(&tv, &tz);
	date += (long) tz.tz_minuteswest * 60;

	/* now fix up local daylight time */
	if (localtime((time_t *)&date)->tm_isdst)
		date -= 60*60;

	return (date);
}

/*
 * gmt_to_local -- take a GMT time expressed in seconds since
 * the epoch, and convert it to local time.
 *
 *	Parameters:	"date" is the number of seconds...
 *
 *	Returns:	Number of seconds corrected to reflect
 *			local time and dst.
 */

long
gmt_to_local(date)
	long	date;
{
	struct	timeval	tv;
	struct	timezone tz;

	(void) gettimeofday(&tv, &tz);
	date -= (long) tz.tz_minuteswest * 60;

	/* now fix up local daylight time */
	if (localtime((time_t *)&date)->tm_isdst)
		date += 60*60;

	return (date);
}

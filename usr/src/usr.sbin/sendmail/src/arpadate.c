/*
 * Copyright (c) 1983 Eric P. Allman
 * Copyright (c) 1988 Regents of the University of California.
 * All rights reserved.
 *
 * %sccs.include.redist.c%
 */

#ifndef lint
static char sccsid[] = "@(#)arpadate.c	6.2 (Berkeley) %G%";
#endif /* not lint */

# include "sendmail.h"
# include <sys/types.h>

/*
**  ARPADATE -- Create date in ARPANET format
**
**	Parameters:
**		ud -- unix style date string.  if NULL, one is created.
**
**	Returns:
**		pointer to an ARPANET date field
**
**	Side Effects:
**		none
**
**	WARNING:
**		date is stored in a local buffer -- subsequent
**		calls will overwrite.
**
**	Bugs:
**		Timezone is computed from local time, rather than
**		from whereever (and whenever) the message was sent.
**		To do better is very hard.
**
**		Some sites are now inserting the timezone into the
**		local date.  This routine should figure out what
**		the format is and work appropriately.
*/

char *
arpadate(ud)
	register char *ud;
{
	register char *p;
	register char *q;
	register int off;
	register int i;
	register struct tm *lt;
	time_t t;
	struct tm gmt;
	static char b[40];
	extern struct tm *localtime(), *gmtime();
	extern char *ctime();
	extern time_t time();

	/*
	**  Get current time.
	**	This will be used if a null argument is passed and
	**	to resolve the timezone.
	*/

	(void) time(&t);
	if (ud == NULL)
		ud = ctime(&t);

	/*
	**  Crack the UNIX date line in a singularly unoriginal way.
	*/

	q = b;

	p = &ud[0];		/* Mon */
	*q++ = *p++;
	*q++ = *p++;
	*q++ = *p++;
	*q++ = ',';
	*q++ = ' ';

	p = &ud[8];		/* 16 */
	if (*p == ' ')
		p++;
	else
		*q++ = *p++;
	*q++ = *p++;
	*q++ = ' ';

	p = &ud[4];		/* Sep */
	*q++ = *p++;
	*q++ = *p++;
	*q++ = *p++;
	*q++ = ' ';

	p = &ud[20];		/* 1979 */
	*q++ = *p++;
	*q++ = *p++;
	*q++ = *p++;
	*q++ = *p++;
	*q++ = ' ';

	p = &ud[11];		/* 01:03:52 */
	for (i = 8; i > 0; i--)
		*q++ = *p++;

	/*
	 * should really get the timezone from the time in "ud" (which
	 * is only different if a non-null arg was passed which is different
	 * from the current time), but for all practical purposes, returning
	 * the current local zone will do (its all that is ever needed).
	 */
	gmt = *gmtime(&t);
	lt = localtime(&t);

	off = (lt->tm_hour - gmt.tm_hour) * 60 + lt->tm_min - gmt.tm_min;

	/* assume that offset isn't more than a day ... */
	if (lt->tm_year < gmt.tm_year)
		off -= 24 * 60;
	else if (lt->tm_year > gmt.tm_year)
		off += 24 * 60;
	else if (lt->tm_yday < gmt.tm_yday)
		off -= 24 * 60;
	else if (lt->tm_yday > gmt.tm_yday)
		off += 24 * 60;

	*q++ = ' ';
	if (off == 0) {
		*q++ = 'G';
		*q++ = 'M';
		*q++ = 'T';
	} else {
		if (off < 0) {
			off = -off;
			*q++ = '-';
		} else
			*q++ = '+';

		if (off >= 24*60)		/* should be impossible */
			off = 23*60+59;		/* if not, insert silly value */

		*q++ = (off / 600) + '0';
		*q++ = (off / 60) % 10 + '0';
		off %= 60;
		*q++ = (off / 10) + '0';
		*q++ = (off % 10) + '0';
	}
	*q = '\0';

	return (b);
}

/*
**  NEXTATOM -- Return pointer to next atom in header
**		(skip whitespace and comments)
**
**	Parameters:
**		s -- pointer to header string
**
**	Returns:
**		pointer advanced to next non-comment header atom
**
**	Side Effects:
**		none
*/

static char *
nextatom(s)
	char *s;
{
	char *p;

	for (p = s;
	     *p && (*p == ' ' || *p == '\t' || *p == '\n' || *p == '(');
	     p++)
	{
		if (*p == '(')
		{
			int nested = 0;

			/* ignore comments */
			p++;
			for (; *p; p++)
			{
				if (*p == '(')
					nested++;
				else if (*p == ')')
					if (!nested)
						break;
					else
						nested--;
			}
		}
	}
	return (p);
}

/*
**  ARPATOUNIX -- Convert RFC-822/1123 date-time specification to ctime format.
**
**	Parameters:
**		s -- pointer to date string
**
**	Returns:
**		pointer to a string in ctime format
**
**	Side Effects:
**		Calls asctime() which modifies its static area.
**
**	Syntax:
**		date-time field specification from RFC822
**		as amended by RFC 1123:
**
**			[ day "," ] 1*2DIGIT month 2*4DIGIT \
**			2DIGIT ":" 2DIGIT [ ":" 2DIGIT  ] zone
**
**		Day can be "Mon" "Tue" "Wed" "Thu" "Fri" "Sat" "Sun"
**			(case-insensitive)
**		Month can be "Jan" "Feb" "Mar" "Apr" "May" "Jun" "Jul"
**			"Aug" "Sep" "Oct" "Nov" "Dec" (also case-insensitive)
**		Zone can be "UT" "GMT" "EST" "EDT" "CST" "CDT" "MST" "MDT"
**			"PST" "PDT" or "+"4*DIGIT or "-"4*DIGIT
**			(case-insensitive; military zones not useful
**			per RFC1123)
**		Additional whitespace or comments may occur.
*/

static char MonthDays[] = {
	31, 28, 31, 30, 31, 30, 31, 31, 30, 31, 30, 31
};

char *
arpatounix(s, e)
	char	*s;
	ENVELOPE *e;
{
	char *p;
	char *n;
	int h_offset = 0;		/* hours */
	int m_offset = 0;		/* minutes */
	struct tm tm;
	extern char *DowList[];		/* defined in collect.c */
	extern char *MonthList[];	/* defined in collect.c */

	bzero((char *) &tm, sizeof tm);
	tm.tm_wday = -1;	/* impossible value */
	p = nextatom (s);

	/* next atom must be a day or a date */
	if (isalpha((int) *p))
	{
		/* day */
		for (tm.tm_wday = 0; DowList[tm.tm_wday]; tm.tm_wday++)
		{
			if (strncasecmp (p, DowList[tm.tm_wday], 3))
				continue;
			else
			{
				p += 3;
				break;
			}
		}
		p = nextatom(p);		/* ',' */
		if (*p == ',')
			p = nextatom(++p);
	}

	/* now must have date */
	tm.tm_mday = atoi(p);
	while (isdigit((int) *p))		/* skip over date */
		p++;
	p = nextatom(p);			/* point to month name */
	for (tm.tm_mon = 0; MonthList[tm.tm_mon]; tm.tm_mon++)
	{
		if (strncasecmp(p, MonthList[tm.tm_mon], 3) == 0)
		{
			p += 3;
			break;
		}
	}
	p = nextatom(p);			/* year */
	tm.tm_year = atoi(p);

	/* if this was 4 digits, subtract 1900 */
	if (tm.tm_year > 999)
		tm.tm_year -= 1900;
	else
	{
		/* if 2 or 3 digits, guess which century and convert */
		time_t now;
		struct tm *gmt;

		(void) time(&now);
		gmt = gmtime(&now);

		/* more likely +1 day than -100(0) years */
		if (gmt->tm_mon == 11 && gmt->tm_mday == 31 &&
		    tm.tm_mon == 0 && tm.tm_mday == 1)
			gmt->tm_year++;
		if (tm.tm_year > 99)
		{
			/* 3 digits */
			tm.tm_year += ((gmt->tm_year + 900 - tm.tm_year) / 1000) * 1000;
		}
		else
		{
			/* 2 digits */
			tm.tm_year += ((gmt->tm_year - tm.tm_year) / 100) * 100;
		}
	}
	while (isdigit((int) *p))	/* skip over year */
		p++;
	p = nextatom(p);		/* hours */
	tm.tm_hour = atoi(p);
	while (isdigit((int) *p))	/* skip over hours */
		p++;
	p = nextatom(p);		/* colon */
	if (*p == ':')
		p = nextatom(++p);
	p = nextatom(p);		/* minutes */
	tm.tm_min = atoi(p);
	while (isdigit((int) *p))	/* skip over minutes */
		p++;
	p = nextatom(p);		/* colon or zone */
	if (*p == ':')			/* have seconds field */
	{
		p = nextatom(++p);
		tm.tm_sec = atoi(p);
		while (isdigit((int) *p))	/* skip over seconds */
			p++;
	}
	p = nextatom(p);		/* zone */
	if (!strncasecmp(p, "UT", 2) || !strncasecmp(p, "GMT", 3))
		;
	else if (!strncasecmp(p, "EDT", 3))
		h_offset = -4;
	else if (!strncasecmp(p, "EST", 3))
	{
		h_offset = -5;
		tm.tm_isdst = 1;
	}
	else if (!strncasecmp(p, "CDT", 3))
		h_offset = -5;
	else if (!strncasecmp(p, "CST", 3))
	{
		h_offset = -6;
		tm.tm_isdst = 1;
	}
	else if (!strncasecmp(p, "MDT", 3))
		h_offset = -6;
	else if (!strncasecmp(p, "MST", 3))
	{
		h_offset = -7;
		tm.tm_isdst = 1;
	}
	else if (!strncasecmp(p, "PDT", 3))
		h_offset = -7;
	else if (!strncasecmp(p, "PST", 3))
	{
		h_offset = -8;
		tm.tm_isdst = 1;
	}
	else if (*p == '+')
	{
		int off;

		off = atoi(++p);
		h_offset = off / 100;
		m_offset = off % 100;
	}
	else if (*p == '-')
	{
		int off;

		off = atoi(++p);
		h_offset = off / -100;
		m_offset = -1 * (off % 100);
	}
	else
	{
#ifdef LOG
		if (LogLevel > 0)
			syslog(LOG_NOTICE, "%s: arpatounix: unparseable date: %s",
				e->e_id, s);
#endif /* LOG */
		return(NULL);
	}

	/* is the year a leap year? */
	if ((tm.tm_year % 4 == 0) &&
	    ((tm.tm_year % 100 != 0) || (tm.tm_year % 400 == 0)))
		MonthDays[2] = 29;
	else
		MonthDays[2] = 28;

	/* apply offset */
	if (h_offset || m_offset)
	{
		tm.tm_min += m_offset;
		tm.tm_hour += h_offset;

		/* normalize */
		if (tm.tm_min < 0)
		{
			tm.tm_hour--;
			tm.tm_min += 60;
		}
		else if (tm.tm_min > 59)
		{
			tm.tm_hour++;
			tm.tm_min -= 60;
		}
		if (tm.tm_hour < 0)
		{
			tm.tm_mday--;
			tm.tm_wday--;
			tm.tm_hour += 24;
		}
		else if (tm.tm_hour > 23)
		{
			tm.tm_mday++;
			tm.tm_wday++;
			tm.tm_hour -= 24;
		}
		if (tm.tm_mday < 1)
		{
			if (--tm.tm_mon == -1)
			{
				tm.tm_mon = 11;
				tm.tm_year--;

				/* is the year a leap year? */
				if ((tm.tm_year % 4 == 0) &&
				    ((tm.tm_year % 100 != 0) || (tm.tm_year % 400 == 0)))
					MonthDays[2] = 29;
				else
					MonthDays[2] = 28;
			}
			tm.tm_mday += MonthDays[tm.tm_mon];
		}
		else if (tm.tm_mday > MonthDays[tm.tm_mon])
		{
			tm.tm_mday -= MonthDays[tm.tm_mon++];
			if (tm.tm_mon > 11)
			{
				tm.tm_mon = 0;
				tm.tm_year++;

				/*
				* Don't have to worry about leap years in
				* January.
				*/
			}
		}
	}

	/* determine day of week if not set from RFC822/1123 line */
	if (tm.tm_wday < 0)
	{
		int i;

		for (i = 0; i < tm.tm_mon; i++)
			tm.tm_yday += MonthDays[i];
		tm.tm_yday += tm.tm_mday;

		/* I wouldn't change these constants if I were you... */
		tm.tm_wday = (int) (((((tm.tm_year + 699L) * 146097L) / 400L) + tm.tm_yday) % 7);
	}

	/* now get UT */
	if ((p = asctime(&tm)) == NULL || *p == '\0' || strlen(p) < 25)
	{
#ifdef LOG
		if (LogLevel > 0)
			syslog(LOG_NOTICE, "%s: arpatounix: asctime failed: %s",
				e->e_id, s);
#endif /* LOG */
		return(NULL);
	}
	if ((n = index(p, '\n')) != NULL)
		*n = '\0';
	return(p);
}

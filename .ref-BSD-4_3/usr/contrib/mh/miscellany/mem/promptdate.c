#include	<stdio.h>
#include	<sys/time.h>
#include	<ctype.h>

/*
**	promptdate
**	prompt user for a date specification which can be quite minimal
**	and print it in a form suitable for parsing by MH
*/

#define		MAXLINE		128

char		*mname[12]	={
		"Jan","Feb","Mar","Apr","May","Jun",
		"Jul","Aug","Sep","Oct","Nov","Dec"};
char		*dname[7]	={
		"Sun","Mon","Tue","Wed","Thu","Fri","Sat"};
struct tm	now;
int		dayspast	= 0;
int		monthlen[2][12]	={
		31,28,31,30,31,30,31,31,30,31,30,31,
		31,29,31,30,31,30,31,31,30,31,30,31};
char		*defaultformat	= "%d %N %y 00:00";

main(argc, argv)
	int		argc;
	char		**argv;
{	
	register int	c;
	struct tm	then;
	extern int	optind;		/* defined in getopt */
	extern char	*optarg;	/* defined in getopt */
	int		getopt();
	long		secsfr70();

	while ((c = getopt (argc, argv, "f:")) != EOF)
	{
		switch (c)
		{
		case 'f':
			defaultformat = optarg;
			break;
		default:
			fprintf(stderr, "usage: %s [-f format] [datespec]\n", argv[0]);
			exit (1);
		}
	}
	argc -= optind;
	argv += optind;

	finddate(&now, dayspast = (int)(secsfr70()/86400L));

	if (argc <= 0)			/* get from user */
	{
		if (!promptdate(&then))
			exit(1);
		printdate(&then, defaultformat);
	}
	else				/* get from command line */
	{
		if (!decodedate(argv[0], &then))
			exit(1);
		printdate(&then, defaultformat);
	}
	exit(0);
}

int promptdate(when)
	struct tm	*when;
{
	char		line[MAXLINE];
	int		decodedate();
	char		*gets();

	for (;;)
	{
		fprintf(stderr, "When? ");
		if (gets(line) == NULL)
		{
			fprintf(stderr, "\n");
			return (0);
		}
		if (decodedate(line, when))
			return (1);
	}
/*NOTREACHED*/
}

int decodedate(line, when)
	char		*line;
	struct tm	*when;
/*
**	accept spec for date in several forms
**	legal are: sun,mon,tue,wed,thu,fri,sat,today,tomorrow,
**	<date><month>,+<relative number of days>
**	<month> should be alpha
**	upper case accepted too
*/
{
	char		s[4];
	register int	i,targetdate;
	int		tem;
	register char	*lptr;

	when->tm_year = now.tm_year;
	when->tm_mon = now.tm_mon;
	targetdate = dayspast;
	for (lptr = line; isspace(*lptr); lptr++)
		;
	if (isdigit(*lptr))
	{
		i = sscanf(lptr, "%d%3s%d", &when->tm_mday, s, &tem);
		switch(i)
		{
		    case 3:
			when->tm_year = tem;
		    case 2:
			fold(s);
			when->tm_mon = monthofyear(s);
			if (i == 3)
				break;
			if (when->tm_mday != 0 && when->tm_mon != 0 && daysfr70(when) < dayspast)
				when->tm_year++;
			break;
		    case 1:
			if (when->tm_mday != 0 && when->tm_mday < now.tm_mday)
			{
				if (++when->tm_mon > 12)
				{
					when->tm_mon = 1;
					when->tm_year++;
				}
			}
		}
		return (validate(when));
	}
	if (isalpha(*lptr))
	{
		sscanf(lptr, "%3s", s);
		fold(s);
		if ((tem = dayofweek(s)) >= 0)
			targetdate += (tem -= now.tm_wday) <= 0 ? tem + 7 : tem;
		else if (strcmp(s, "Tom") == 0)
			targetdate++;
		else if (strcmp(s, "Tod") == 0)
			;
		else	/* mistake */
			return (0);
	}
	else if (*lptr == '+')
	{
		if (sscanf(++lptr, "%d", &tem) == 0 || tem < 0)	/* mistake */
			return (0);
		targetdate += tem;
	}
	else	/* mistake by default */
		return (0);
	finddate(when, targetdate);
	return (when->tm_mday != 0);
}

int validate(datetm)
/*
**	check that a given date and month combination is legal
**	datetm->tm_year must hold the year in question
*/
	register struct tm	*datetm;
{

	return (datetm->tm_mday <= monthlen[leapyear(datetm->tm_year)]
		[datetm->tm_mon] && datetm->tm_mday > 0);
}

finddate(datetm, df70)
/*
**	convert days from 1 jan 1970 to a date in struct datetm
*/
	register int		df70;
	register struct tm	*datetm;
{
	register struct tm	*tdtm;
	long			longtime;
	struct tm		*gmtime();

	longtime = df70 * 86400L;
	tdtm = gmtime(&longtime);
	datetm->tm_yday = tdtm->tm_yday;
	datetm->tm_wday = tdtm->tm_wday;
	datetm->tm_year = tdtm->tm_year + 1900;
	datetm->tm_mon = tdtm->tm_mon;
	datetm->tm_mday = tdtm->tm_mday;
	datetm->tm_hour = tdtm->tm_hour;
	datetm->tm_min = tdtm->tm_min;
	datetm->tm_sec = tdtm->tm_sec;
}

fold(s)
/*
**	convert first character to uppercase
**	convert rest of string from uppercase to lower case
*/
	register char	*s;
{
	register char	c;

	if ((c = *s) != '\0')
		*s++ += islower(c) ? 'A' - 'a' : 0;
	while ((c = *s) != '\0')
		*s++ += isupper(c) ? 'a' - 'A' : 0;
}

int leapyear(y)
/*
**	returns 1 if leapyear 0 otherwise
*/
	register int	y;
{

	return (((y % 4) == 0 && (y % 100) != 0) || (y % 400) == 0);
}

int daysfr70(datetm)
/*
**	returns the number of days from 1 Jan 1970
**	no checking for illegal date at all
*/
	register struct tm	*datetm;
{
	register int		i, totdays;


	totdays = 0;
	for (i = 1970; i <= 2050 && i < datetm->tm_year; i++)	/* prevent overflow */
		totdays += 365 + leapyear(i);
	for (i = 0; i < 12 && i < datetm->tm_mon; i++)
		totdays += monthlen[leapyear(datetm->tm_year)][i];
	totdays += datetm->tm_mday - 1;
	return (totdays);
}

int monthofyear(s)
/*
**	returns month of year in numeric form when given
**	the first three letters
*/
	register char	*s;
{
	register int	i;

	fold(s);
	for (i = 12; i-- && strcmp(s,mname[i]); )
		;
	return (i);
}

int dayofweek(s)
/*
**	sunday = 0,...,saturday = 6, nomatch = -1
*/
	register char	*s;
{
	register int	i;

	fold(s);
	for (i = 7; i-- && strcmp(s,dname[i]); )
		;
	return (i);
}

printdate(date, format)
/*
**	print date in MH acceptable format
**	kludge - general formats are not implemented
*/
	struct tm	*date;
	char		*format;
{
	printf("%d %s %d 00:00\n",
		date->tm_mday, mname[date->tm_mon], date->tm_year);
}

long secsfr70()
/*
**	This is system dependent
*/
{
	register int		dst;
	struct timeval		tv;
	struct timezone		tz;
	struct tm		*localtime();

	gettimeofday(&tv, &tz);
	dst = localtime(&tv.tv_sec)->tm_isdst;
	return (tv.tv_sec - tz.tz_minuteswest * 60 + (dst ? 3600 : 0));
}

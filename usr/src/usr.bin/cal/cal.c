/*
 * Copyright (c) 1989 The Regents of the University of California.
 * All rights reserved.
 *
 * This code is derived from software contributed to Berkeley by
 * Kim Letkeman.
 *
 * Redistribution and use in source and binary forms are permitted
 * provided that the above copyright notice and this paragraph are
 * duplicated in all such forms and that any documentation,
 * advertising materials, and other materials related to such
 * distribution and use acknowledge that the software was developed
 * by the University of California, Berkeley.  The name of the
 * University may not be used to endorse or promote products derived
 * from this software without specific prior written permission.
 * THIS SOFTWARE IS PROVIDED ``AS IS'' AND WITHOUT ANY EXPRESS OR
 * IMPLIED WARRANTIES, INCLUDING, WITHOUT LIMITATION, THE IMPLIED
 * WARRANTIES OF MERCHANTABILITY AND FITNESS FOR A PARTICULAR PURPOSE.
 */

#ifndef lint
char copyright[] =
"@(#) Copyright (c) 1989 The Regents of the University of California.\n\
 All rights reserved.\n";
#endif /* not lint */

#ifndef lint
static char sccsid[] = "@(#)cal.c	4.6 (Berkeley) %G%";
#endif /* not lint */

#include <sys/types.h>
#include <sys/time.h>
#include <stdio.h>
#include <ctype.h>

#define	THURSDAY		4		/* for reformation */
#define	SATURDAY 		6		/* 1 Jan 1 was a Saturday */
#define	FIRST_MISSING_DAY 	639787		/* 3 Sep 1752 */
#define	NUMBER_MISSING_DAYS 	11		/* 11 day correction */
#define	SPACE			99		/* used in day array */

static int days_in_month[2][13] = {
	{0, 31, 28, 31, 30, 31, 30, 31, 31, 30, 31, 30, 31},
	{0, 31, 29, 31, 30, 31, 30, 31, 31, 30, 31, 30, 31},
};

static int sep1752[42] = {
	SPACE, SPACE,     1,     2,    14,    15,    16,
	   17,    18,    19,    20,    21,    22,    23,
	   24,    25,    26,    27,    28,    29,    30,
	SPACE, SPACE, SPACE, SPACE, SPACE, SPACE, SPACE,
	SPACE, SPACE, SPACE, SPACE, SPACE, SPACE, SPACE,
	SPACE, SPACE, SPACE, SPACE, SPACE, SPACE, SPACE,
};

static char *month_names[12] = {
	"January", "February", "March", "April", "May", "June",
	"July", "August", "September", "October", "November", "December",
};

static char *day_headings = " S  M Tu  W Th  F  S";

/* leap year -- account for gregorian reformation in 1752 */
#define	leap_year(yr) \
	((yr) <= 1752 ? !((yr) % 4) : \
	!((yr) % 4) && ((yr) % 100) || !((yr) % 400))

/* number of centuries since 1700, not inclusive */
#define	centuries_since_1700(yr) \
	((yr) > 1700 ? (yr) / 100 - 17 : 0)

/* number of centuries since 1700 whose modulo of 400 is 0 */
#define	quad_centuries_since_1700(yr) \
	((yr) > 1600 ? ((yr) - 1600) / 400 : 0)

/* number of leap years between year 1 and this year, not inclusive */
#define	leap_years_since_year_1(yr) \
	((yr) / 4 - centuries_since_1700(yr) + quad_centuries_since_1700(yr))

main(argc, argv)
	int argc;
	char **argv;
{
	extern char *optarg;
	extern int optind;
	struct tm *local_time;
	time_t now, time();
	int ch, month, year, yflag;

	yflag = 0;
	while ((ch = getopt(argc, argv, "y")) != EOF)
		switch(ch) {
		case 'y':
			yflag = 1;
			break;
		case '?':
		default:
			usage();
		}
	argc -= optind;
	argv += optind;

	switch(argc) {
	case 2:
		if ((month = atoi(*argv++)) <= 0 || month > 12) {
			(void)fprintf(stderr, "cal: illegal month value.\n");
			exit(1);
		}
		/* FALLTHROUGH */
	case 1:
		if ((year = atoi(*argv)) <= 0 || year > 9999) {
			(void)fprintf(stderr, "cal: illegal year value.\n");
			exit(1);
		}
		break;
	case 0:
		(void)time(&now);
		local_time = localtime(&now);
		year = local_time->tm_year + 1900;
		if (!yflag)
			month = local_time->tm_mon + 1;
		break;
	default:
		usage();
	}
	if (month)
		print_monthly_calendar(month, year);
	else
		print_yearly_calendar(year);
	exit(0);
}

print_monthly_calendar(month, year)
	int month, year;
{
	register int col, row;
	register char *p;
	int days[42];
	char lineout[30];

	generate_day_array(month, year, days);
	(void)printf("   %s %d\n%s\n",
	    month_names[month - 1], year, day_headings);
	for (row = 0; row < 6; row++) {
		for (col = 0, p = lineout; col < 7; col++, p += 3)
			ascii_day(p, days[row * 7 + col]);
		trim_trailing_spaces(lineout);
		(void)printf("%s\n", lineout);
	}
}

print_yearly_calendar(year)
	int year;
{
	register int col, *dp, i, month, row, which_cal;
	register char *p;
	int days[12][42];
	char lineout[80];

	(void)printf("\n\n\n\t\t\t\t%d\n\n", year);
	for (i = 0; i < 12; i++)
		generate_day_array(i + 1, year, &days[i][0]);
	(void)memset(lineout, ' ', sizeof(lineout) - 1);
	lineout[sizeof(lineout) - 1] = '\0';
	for (month = 0; month < 12; month += 3) {
		(void)printf("\t %.3s\t\t\t%.3s\t\t       %.3s\n",
		    month_names[month], month_names[month + 1],
		    month_names[month + 2]);
		for (i = 0; i < 3; i++) {
			(void)printf("%s", day_headings);
			if (i < 2)
				(void)printf("   ");
		}
		(void)printf("\n");
		for (row = 0; row < 6; row++) {
			for (which_cal = 0; which_cal < 3; which_cal++) {
				p = lineout + which_cal * 23;
				dp = &days[month + which_cal][row * 7];
				for (col = 0; col < 7; col++, p += 3)
					ascii_day(p, *dp++);
			}
			trim_trailing_spaces(lineout);
			(void)printf("%s\n", lineout);
		}
	}
	(void)printf("\n\n\n");
}

/*
 * generate_day_array --
 *	Fill in an array of 42 integers with a calendar.  Assume for a moment
 *	that you took the (maximum) 6 rows in a calendar and stretched them
 *	out end to end.  You would have 42 numbers or spaces.  This routine
 *	builds that array for any month from Jan. 1 through Dec. 9999.
 */
generate_day_array(month, year, days)
	int month, year, *days;
{
	register int i, day, dw, dm;

	dm = days_in_month[leap_year(year)][month];
	dw = day_in_week(1, month, year);
	if (month == 9 && year == 1752)
		for (i = 0; i < 42; i++)
			days[i] = sep1752[i];
	else {
		for (i = 0; i < 42; i++)
			days[i] = SPACE;
		for (day = 1, i = dw; day <= dm; ++day, ++i)
			days[i] = day;
	}
}

/*
 * day_in_year --
 *	return the 1 based day number within the year
 */
day_in_year(day, month, year)
	register int day, month;
	int year;
{
	register int i, leap;

	leap = leap_year(year);
	for (i = 1; i < month; i++)
		day += days_in_month[leap][i];
	return(day);
}

/*
 * day_in_week
 *	return the 0 based day number for any date from 1 Jan. 1 to
 *	31 Dec. 9999.  Assumes the Gregorian reformation eliminates
 *	3 Sep. 1752 through 13 Sep. 1752.  Returns Thursday for all
 *	missing days.
 */
day_in_week(day, month, year)
	int day, month, year;
{
	long temp;

	temp = (long)(year - 1) * 365 + leap_years_since_year_1(year - 1)
	    + day_in_year(day, month, year);
	if (temp < FIRST_MISSING_DAY)
		return((temp - 1 + SATURDAY) % 7);
	if (temp >= (FIRST_MISSING_DAY + NUMBER_MISSING_DAYS))
		return(((temp - 1 + SATURDAY) - NUMBER_MISSING_DAYS) % 7);
	return(THURSDAY);
}

ascii_day(p, day)
	register char *p;
	register int day;
{
	register int ten;

	if (day == SPACE) {
		*p++ = ' ';
		*p++ = ' ';
	}
	else {
		ten = day / 10;
		*p++ = ten ? ten + '0' : ' ';
		*p++ = day % 10 + '0';
	}
	*p = ' ';
}

trim_trailing_spaces(s)
	register char *s;
{
	register char *p;

	for (p = s; *p; ++p);
	while (p > s && isspace(*--p));
	if (p > s)
		++p;
	*p = '\0';
}

usage()
{
	(void)fprintf(stderr, "usage: cal [-y] [[month] year]\n");
	exit(1);
}

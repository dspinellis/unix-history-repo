/*
 * strftime.c
 *
 * Public-domain relatively quick-and-dirty implementation of
 * ANSI library routine for System V Unix systems.
 *
 * It's written in old-style C for maximal portability.
 * However, since I'm used to prototypes, I've included them too.
 *
 * If you want stuff in the System V ascftime routine, add the SYSV_EXT define.
 * For extensions from SunOS, add SUNOS_EXT.
 * For stuff needed to implement the P1003.2 date command, add POSIX2_DATE.
 * For complete POSIX semantics, add POSIX_SEMANTICS.
 *
 * The code for %c, %x, and %X is my best guess as to what's "appropriate".
 * This version ignores LOCALE information.
 * It also doesn't worry about multi-byte characters.
 * So there.
 *
 * This file is also shipped with GAWK (GNU Awk), gawk specific bits of
 * code are included if GAWK is defined.
 *
 * Arnold Robbins
 * January, February, March, 1991
 * Updated March, April 1992
 * Updated April, 1993
 *
 * Fixes from ado@elsie.nci.nih.gov
 * February 1991, May 1992
 * Fixes from Tor Lillqvist tor@tik.vtt.fi
 * May, 1993
 */

#ifndef GAWK
#include <stdio.h>
#include <ctype.h>
#include <string.h>
#include <time.h>
#include <sys/types.h>
#endif

/* defaults: season to taste */
#define SYSV_EXT	1	/* stuff in System V ascftime routine */
#define SUNOS_EXT	1	/* stuff in SunOS strftime routine */
#define POSIX2_DATE	1	/* stuff in Posix 1003.2 date command */
#define VMS_EXT		1	/* include %v for VMS date format */
#ifndef GAWK
#define POSIX_SEMANTICS	1	/* call tzset() if TZ changes */
#endif

#if defined(POSIX2_DATE)
#if ! defined(SYSV_EXT)
#define SYSV_EXT	1
#endif
#if ! defined(SUNOS_EXT)
#define SUNOS_EXT	1
#endif
#endif

#if defined(POSIX2_DATE)
#define adddecl(stuff)	stuff
#else
#define adddecl(stuff)
#endif

#undef strchr	/* avoid AIX weirdness */

#ifndef __STDC__
#define const	/**/
extern void *malloc();
extern void *realloc();
extern void tzset();
extern char *strchr();
extern char *getenv();
static int weeknumber();
adddecl(static int iso8601wknum();)
#else
extern void *malloc(unsigned count);
extern void *realloc(void *ptr, unsigned count);
extern void tzset(void);
extern char *strchr(const char *str, int ch);
extern char *getenv(const char *v);
static int weeknumber(const struct tm *timeptr, int firstweekday);
adddecl(static int iso8601wknum(const struct tm *timeptr);)
#endif

#ifdef __GNUC__
#define inline	__inline__
#else
#define inline	/**/
#endif

#define range(low, item, hi)	max(low, min(item, hi))

#if !defined(MSDOS) && !defined(TZNAME_MISSING)
extern char *tzname[2];
extern int daylight;
#endif

/* min --- return minimum of two numbers */

#ifndef __STDC__
static inline int
min(a, b)
int a, b;
#else
static inline int
min(int a, int b)
#endif
{
	return (a < b ? a : b);
}

/* max --- return maximum of two numbers */

#ifndef __STDC__
static inline int
max(a, b)
int a, b;
#else
static inline int
max(int a, int b)
#endif
{
	return (a > b ? a : b);
}

/* strftime --- produce formatted time */

#ifndef __STDC__
size_t
strftime(s, maxsize, format, timeptr)
char *s;
size_t maxsize;
const char *format;
const struct tm *timeptr;
#else
size_t
strftime(char *s, size_t maxsize, const char *format, const struct tm *timeptr)
#endif
{
	char *endp = s + maxsize;
	char *start = s;
	char tbuf[100];
	int i;
	static short first = 1;
#ifdef POSIX_SEMANTICS
	static char *savetz = NULL;
	static int savetzlen = 0;
	char *tz;
#endif /* POSIX_SEMANTICS */

	/* various tables, useful in North America */
	static char *days_a[] = {
		"Sun", "Mon", "Tue", "Wed",
		"Thu", "Fri", "Sat",
	};
	static char *days_l[] = {
		"Sunday", "Monday", "Tuesday", "Wednesday",
		"Thursday", "Friday", "Saturday",
	};
	static char *months_a[] = {
		"Jan", "Feb", "Mar", "Apr", "May", "Jun",
		"Jul", "Aug", "Sep", "Oct", "Nov", "Dec",
	};
	static char *months_l[] = {
		"January", "February", "March", "April",
		"May", "June", "July", "August", "September",
		"October", "November", "December",
	};
	static char *ampm[] = { "AM", "PM", };

	if (s == NULL || format == NULL || timeptr == NULL || maxsize == 0)
		return 0;

	if (strchr(format, '%') == NULL && strlen(format) + 1 >= maxsize)
		return 0;

#ifndef POSIX_SEMANTICS
	if (first) {
		tzset();
		first = 0;
	}
#else	/* POSIX_SEMANTICS */
	tz = getenv("TZ");
	if (first) {
		if (tz != NULL) {
			int tzlen = strlen(tz);

			savetz = (char *) malloc(tzlen + 1);
			if (savetz != NULL) {
				savetzlen = tzlen + 1;
				strcpy(savetz, tz);
			}
		}
		tzset();
		first = 0;
	}
	/* if we have a saved TZ, and it is different, recapture and reset */
	if (tz && savetz && (tz[0] != savetz[0] || strcmp(tz, savetz) != 0)) {
		i = strlen(tz) + 1;
		if (i > savetzlen) {
			savetz = (char *) realloc(savetz, i);
			if (savetz) {
				savetzlen = i;
				strcpy(savetz, tz);
			}
		} else
			strcpy(savetz, tz);
		tzset();
	}
#endif	/* POSIX_SEMANTICS */

	for (; *format && s < endp - 1; format++) {
		tbuf[0] = '\0';
		if (*format != '%') {
			*s++ = *format;
			continue;
		}
	again:
		switch (*++format) {
		case '\0':
			*s++ = '%';
			goto out;

		case '%':
			*s++ = '%';
			continue;

		case 'a':	/* abbreviated weekday name */
			if (timeptr->tm_wday < 0 || timeptr->tm_wday > 6)
				strcpy(tbuf, "?");
			else
				strcpy(tbuf, days_a[timeptr->tm_wday]);
			break;

		case 'A':	/* full weekday name */
			if (timeptr->tm_wday < 0 || timeptr->tm_wday > 6)
				strcpy(tbuf, "?");
			else
				strcpy(tbuf, days_l[timeptr->tm_wday]);
			break;

#ifdef SYSV_EXT
		case 'h':	/* abbreviated month name */
#endif
		case 'b':	/* abbreviated month name */
			if (timeptr->tm_mon < 0 || timeptr->tm_mon > 11)
				strcpy(tbuf, "?");
			else
				strcpy(tbuf, months_a[timeptr->tm_mon]);
			break;

		case 'B':	/* full month name */
			if (timeptr->tm_mon < 0 || timeptr->tm_mon > 11)
				strcpy(tbuf, "?");
			else
				strcpy(tbuf, months_l[timeptr->tm_mon]);
			break;

		case 'c':	/* appropriate date and time representation */
			sprintf(tbuf, "%s %s %2d %02d:%02d:%02d %d",
				days_a[range(0, timeptr->tm_wday, 6)],
				months_a[range(0, timeptr->tm_mon, 11)],
				range(1, timeptr->tm_mday, 31),
				range(0, timeptr->tm_hour, 23),
				range(0, timeptr->tm_min, 59),
				range(0, timeptr->tm_sec, 61),
				timeptr->tm_year + 1900);
			break;

		case 'd':	/* day of the month, 01 - 31 */
			i = range(1, timeptr->tm_mday, 31);
			sprintf(tbuf, "%02d", i);
			break;

		case 'H':	/* hour, 24-hour clock, 00 - 23 */
			i = range(0, timeptr->tm_hour, 23);
			sprintf(tbuf, "%02d", i);
			break;

		case 'I':	/* hour, 12-hour clock, 01 - 12 */
			i = range(0, timeptr->tm_hour, 23);
			if (i == 0)
				i = 12;
			else if (i > 12)
				i -= 12;
			sprintf(tbuf, "%02d", i);
			break;

		case 'j':	/* day of the year, 001 - 366 */
			sprintf(tbuf, "%03d", timeptr->tm_yday + 1);
			break;

		case 'm':	/* month, 01 - 12 */
			i = range(0, timeptr->tm_mon, 11);
			sprintf(tbuf, "%02d", i + 1);
			break;

		case 'M':	/* minute, 00 - 59 */
			i = range(0, timeptr->tm_min, 59);
			sprintf(tbuf, "%02d", i);
			break;

		case 'p':	/* am or pm based on 12-hour clock */
			i = range(0, timeptr->tm_hour, 23);
			if (i < 12)
				strcpy(tbuf, ampm[0]);
			else
				strcpy(tbuf, ampm[1]);
			break;

		case 'S':	/* second, 00 - 61 */
			i = range(0, timeptr->tm_sec, 61);
			sprintf(tbuf, "%02d", i);
			break;

		case 'U':	/* week of year, Sunday is first day of week */
			sprintf(tbuf, "%d", weeknumber(timeptr, 0));
			break;

		case 'w':	/* weekday, Sunday == 0, 0 - 6 */
			i = range(0, timeptr->tm_wday, 6);
			sprintf(tbuf, "%d", i);
			break;

		case 'W':	/* week of year, Monday is first day of week */
			sprintf(tbuf, "%d", weeknumber(timeptr, 1));
			break;

		case 'x':	/* appropriate date representation */
			sprintf(tbuf, "%s %s %2d %d",
				days_a[range(0, timeptr->tm_wday, 6)],
				months_a[range(0, timeptr->tm_mon, 11)],
				range(1, timeptr->tm_mday, 31),
				timeptr->tm_year + 1900);
			break;

		case 'X':	/* appropriate time representation */
			sprintf(tbuf, "%02d:%02d:%02d",
				range(0, timeptr->tm_hour, 23),
				range(0, timeptr->tm_min, 59),
				range(0, timeptr->tm_sec, 61));
			break;

		case 'y':	/* year without a century, 00 - 99 */
			i = timeptr->tm_year % 100;
			sprintf(tbuf, "%d", i);
			break;

		case 'Y':	/* year with century */
			sprintf(tbuf, "%d", 1900 + timeptr->tm_year);
			break;

		case 'Z':	/* time zone name or abbrevation */
			i = 0;
			if (
#ifndef TZNAME_MISSING
			    daylight &&
#endif
			    timeptr->tm_isdst)
				i = 1;
#ifdef TZNAME_MISSING
			strcpy(tbuf, timeptr->tm_zone);
#else
			strcpy(tbuf, tzname[i]);
#endif
			break;

#ifdef SYSV_EXT
		case 'n':	/* same as \n */
			tbuf[0] = '\n';
			tbuf[1] = '\0';
			break;

		case 't':	/* same as \t */
			tbuf[0] = '\t';
			tbuf[1] = '\0';
			break;

		case 'D':	/* date as %m/%d/%y */
			strftime(tbuf, sizeof tbuf, "%m/%d/%y", timeptr);
			break;

		case 'e':	/* day of month, blank padded */
			sprintf(tbuf, "%2d", range(1, timeptr->tm_mday, 31));
			break;

		case 'r':	/* time as %I:%M:%S %p */
			strftime(tbuf, sizeof tbuf, "%I:%M:%S %p", timeptr);
			break;

		case 'R':	/* time as %H:%M */
			strftime(tbuf, sizeof tbuf, "%H:%M", timeptr);
			break;

		case 'T':	/* time as %H:%M:%S */
			strftime(tbuf, sizeof tbuf, "%H:%M:%S", timeptr);
			break;
#endif

#ifdef SUNOS_EXT
		case 'k':	/* hour, 24-hour clock, blank pad */
			sprintf(tbuf, "%2d", range(0, timeptr->tm_hour, 23));
			break;

		case 'l':	/* hour, 12-hour clock, 1 - 12, blank pad */
			i = range(0, timeptr->tm_hour, 23);
			if (i == 0)
				i = 12;
			else if (i > 12)
				i -= 12;
			sprintf(tbuf, "%2d", i);
			break;
#endif


#ifdef VMS_EXT
		case 'v':	/* date as dd-bbb-YYYY */
			sprintf(tbuf, "%2d-%3.3s-%4d",
				range(1, timeptr->tm_mday, 31),
				months_a[range(0, timeptr->tm_mon, 11)],
				timeptr->tm_year + 1900);
			for (i = 3; i < 6; i++)
				if (islower(tbuf[i]))
					tbuf[i] = toupper(tbuf[i]);
			break;
#endif


#ifdef POSIX2_DATE
		case 'C':
			sprintf(tbuf, "%02d", (timeptr->tm_year + 1900) / 100);
			break;


		case 'E':
		case 'O':
			/* POSIX locale extensions, ignored for now */
			goto again;

		case 'V':	/* week of year according ISO 8601 */
#if defined(GAWK) && defined(VMS_EXT)
		{
			extern int do_lint;
			extern void warning();
			static int warned = 0;

			if (! warned && do_lint) {
				warned = 1;
				warning(
	"conversion %%V added in P1003.2/11.3; for VMS style date, use %%v");
			}
		}
#endif
			sprintf(tbuf, "%d", iso8601wknum(timeptr));
			break;

		case 'u':
		/* ISO 8601: Weekday as a decimal number [1 (Monday) - 7] */
			sprintf(tbuf, "%d", timeptr->tm_wday == 0 ? 7 :
					timeptr->tm_wday);
			break;
#endif	/* POSIX2_DATE */
		default:
			tbuf[0] = '%';
			tbuf[1] = *format;
			tbuf[2] = '\0';
			break;
		}
		i = strlen(tbuf);
		if (i)
			if (s + i < endp - 1) {
				strcpy(s, tbuf);
				s += i;
			} else
				return 0;
	}
out:
	if (s < endp && *format == '\0') {
		*s = '\0';
		return (s - start);
	} else
		return 0;
}

#ifdef POSIX2_DATE
/* iso8601wknum --- compute week number according to ISO 8601 */

#ifndef __STDC__
static int
iso8601wknum(timeptr)
const struct tm *timeptr;
#else
static int
iso8601wknum(const struct tm *timeptr)
#endif
{
	/*
	 * From 1003.2 D11.3:
	 *	If the week (Monday to Sunday) containing January 1
	 *	has four or more days in the new year, then it is week 1;
	 *	otherwise it is week 53 of the previous year, and the
	 *	next week is week 1.
	 *
	 * ADR: This means if Jan 1 was Monday through Thursday,
	 *	it was week 1, otherwise week 53.
	 */

	int simple_wknum, jan1day, diff, ret;

	/* get week number, Monday as first day of the week */
	simple_wknum = weeknumber(timeptr, 1) + 1;

	/*
	 * With thanks and tip of the hatlo to tml@tik.vtt.fi
	 *
	 * What day of the week does January 1 fall on?
	 * We know that
	 *	(timeptr->tm_yday - jan1.tm_yday) MOD 7 ==
	 *		(timeptr->tm_wday - jan1.tm_wday) MOD 7
	 * and that
	 * 	jan1.tm_yday == 0
	 * and that
	 * 	timeptr->tm_wday MOD 7 == timeptr->tm_wday
	 * from which it follows that. . .
 	 */
	jan1day = timeptr->tm_wday - (timeptr->tm_yday % 7);
	if (jan1day < 0)
		jan1day += 7;

	/*
	 * If Jan 1 was a Monday through Thursday, it was in
	 * week 1.  Otherwise it was last year's week 53, which is
	 * this year's week 0.
	 */
	if (jan1day >= 1 && jan1day <= 4)
		diff = 0;
	else
		diff = 1;
	ret = simple_wknum - diff;
	if (ret == 0)	/* we're in the first week of the year */
		ret = 53;
	return ret;
}
#endif

/* weeknumber --- figure how many weeks into the year */

/* With thanks and tip of the hatlo to ado@elsie.nci.nih.gov */

#ifndef __STDC__
static int
weeknumber(timeptr, firstweekday)
const struct tm *timeptr;
int firstweekday;
#else
static int
weeknumber(const struct tm *timeptr, int firstweekday)
#endif
{
	if (firstweekday == 0)
		return (timeptr->tm_yday + 7 - timeptr->tm_wday) / 7;
	else
		return (timeptr->tm_yday + 7 -
			(timeptr->tm_wday ? (timeptr->tm_wday - 1) : 6)) / 7;
}

#if 0
/* ADR --- I'm loathe to mess with ado's code ... */

Date:         Wed, 24 Apr 91 20:54:08 MDT
From: Michal Jaegermann <audfax!emory!vm.ucs.UAlberta.CA!NTOMCZAK>
To: arnold@audiofax.com

Hi Arnold,
in a process of fixing of strftime() in libraries on Atari ST I grabbed
some pieces of code from your own strftime.  When doing that it came
to mind that your weeknumber() function compiles a little bit nicer
in the following form:
/*
 * firstweekday is 0 if starting in Sunday, non-zero if in Monday
 */
{
    return (timeptr->tm_yday - timeptr->tm_wday +
	    (firstweekday ? (timeptr->tm_wday ? 8 : 1) : 7)) / 7;
}
How nicer it depends on a compiler, of course, but always a tiny bit.

   Cheers,
   Michal
   ntomczak@vm.ucs.ualberta.ca
#endif

#ifdef	TEST_STRFTIME

/*
 * NAME:
 *	tst
 *
 * SYNOPSIS:
 *	tst
 *
 * DESCRIPTION:
 *	"tst" is a test driver for the function "strftime".
 *
 * OPTIONS:
 *	None.
 *
 * AUTHOR:
 *	Karl Vogel
 *	Control Data Systems, Inc.
 *	vogelke@c-17igp.wpafb.af.mil
 *
 * BUGS:
 *	None noticed yet.
 *
 * COMPILE:
 *	cc -o tst -DTEST_STRFTIME strftime.c
 */

/* ADR: I reformatted this to my liking, and deleted some unneeded code. */

#ifndef NULL
#include	<stdio.h>
#endif
#include	<sys/time.h>
#include	<string.h>

#define		MAXTIME		132

/*
 * Array of time formats.
 */

static char *array[] =
{
	"(%%A)      full weekday name, var length (Sunday..Saturday)  %A",
	"(%%B)       full month name, var length (January..December)  %B",
	"(%%C)                                               Century  %C",
	"(%%D)                                       date (%%m/%%d/%%y)  %D",
	"(%%E)                           Locale extensions (ignored)  %E",
	"(%%H)                          hour (24-hour clock, 00..23)  %H",
	"(%%I)                          hour (12-hour clock, 01..12)  %I",
	"(%%M)                                       minute (00..59)  %M",
	"(%%O)                           Locale extensions (ignored)  %O",
	"(%%R)                                 time, 24-hour (%%H:%%M)  %R",
	"(%%S)                                       second (00..61)  %S",
	"(%%T)                              time, 24-hour (%%H:%%M:%%S)  %T",
	"(%%U)    week of year, Sunday as first day of week (00..53)  %U",
	"(%%V)                    week of year according to ISO 8601  %V",
	"(%%W)    week of year, Monday as first day of week (00..53)  %W",
	"(%%X)     appropriate locale time representation (%H:%M:%S)  %X",
	"(%%Y)                           year with century (1970...)  %Y",
	"(%%Z) timezone (EDT), or blank if timezone not determinable  %Z",
	"(%%a)          locale's abbreviated weekday name (Sun..Sat)  %a",
	"(%%b)            locale's abbreviated month name (Jan..Dec)  %b",
	"(%%c)           full date (Sat Nov  4 12:02:33 1989)%n%t%t%t  %c",
	"(%%d)                             day of the month (01..31)  %d",
	"(%%e)               day of the month, blank-padded ( 1..31)  %e",
	"(%%h)                                should be same as (%%b)  %h",
	"(%%j)                            day of the year (001..366)  %j",
	"(%%k)               hour, 24-hour clock, blank pad ( 0..23)  %k",
	"(%%l)               hour, 12-hour clock, blank pad ( 0..12)  %l",
	"(%%m)                                        month (01..12)  %m",
	"(%%p)              locale's AM or PM based on 12-hour clock  %p",
	"(%%r)                   time, 12-hour (same as %%I:%%M:%%S %%p)  %r",
	"(%%u) ISO 8601: Weekday as decimal number [1 (Monday) - 7]   %u",
	"(%%v)                                VAX date (dd-bbb-YYYY)  %v",
	"(%%w)                       day of week (0..6, Sunday == 0)  %w",
	"(%%x)                appropriate locale date representation  %x",
	"(%%y)                      last two digits of year (00..99)  %y",
	(char *) NULL
};

/* Main routine. */

int
main(argc, argv)
int argc;
char **argv;
{
	long time();

	char *next;
	char string[MAXTIME];

	int k;
	int length;

	struct tm *tm;

	long clock;

	/* Call the function. */

	clock = time((long *) 0);
	tm = localtime(&clock);

	for (k = 0; next = array[k]; k++) {
		length = strftime(string, MAXTIME, next, tm);
		printf("%s\n", string);
	}

	exit(0);
}
#endif	/* TEST_STRFTIME */

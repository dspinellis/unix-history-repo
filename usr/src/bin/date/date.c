#ifndef lint
static char *sccsid = "@(#)date.c	4.7 (Berkeley) %G%";
#endif

/*
 * Date - print and set date
 */

#include <stdio.h>
#include <sys/time.h>
#include <utmp.h>
#define WTMP "/usr/adm/wtmp"

struct	timeval tv;
struct	timezone tz;
char	*ap, *ep, *sp;
int	uflag;

char	*timezone();
static	int	dmsize[12] =
    { 31, 28, 31, 30, 31, 30, 31, 31, 30, 31, 30, 31 };
static char *usage = "usage: date [-u] [yymmddhhmm[.ss]]\n";

struct utmp wtmp[2] = {
	{ "|", "", "", 0 },
	{ "{", "", "", 0 }
};

char	*ctime();
char	*asctime();
struct	tm *localtime();
struct	tm *gmtime();

main(argc, argv)
	int argc;
	char *argv[];
{
	register char *tzn;
	int wf, rc;

	rc = 0;
	gettimeofday(&tv, &tz);
	if (argc > 1 && strcmp(argv[1], "-u") == 0) {
		argc--;
		argv++;
		uflag++;
	}
	if (argc > 1) {
		ap = argv[1];
		wtmp[0].ut_time = tv.tv_sec;
		if (gtime()) {
			printf(usage);
			exit(1);
		}
		/* convert to GMT assuming local time */
		if (uflag == 0) {
			tv.tv_sec += (long)tz.tz_minuteswest*60;
			/* now fix up local daylight time */
			if (localtime(&tv.tv_sec)->tm_isdst)
				tv.tv_sec -= 60*60;
		}
		tv.tv_sec = tv.tv_sec;
		if (settimeofday(&tv, (struct timezone *)0) < 0) {
			rc++;
			perror("Failed to set date");
		} else if ((wf = open(WTMP, 1)) >= 0) {
			time(&wtmp[1].ut_time);
			lseek(wf, 0L, 2);
			write(wf, (char *)wtmp, sizeof(wtmp));
			close(wf);
		}
	}
	if (rc == 0)
		time(&tv.tv_sec);
	if (uflag) {
		ap = asctime(gmtime(&tv.tv_sec));
		tzn = "GMT";
	} else {
		struct tm *tp;
		tp = localtime(&tv.tv_sec);
		ap = asctime(tp);
		tzn = timezone(tz.tz_minuteswest, tp->tm_isdst);
	}
	printf("%.20s", ap);
	if (tzn)
		printf("%s", tzn);
	printf("%s", ap+19);
	exit(rc);
}

gtime()
{
	register int i, year, month;
	int day, hour, mins, secs;
	struct tm *L;
	char x;

	ep = ap;
	while(*ep) ep++;
	sp = ap;
	while(sp < ep) {
		x = *sp;
		*sp++ = *--ep;
		*ep = x;
	}
	sp = ap;
	gettimeofday(&tv, 0);
	L = localtime(&tv.tv_sec);
	secs = gp(-1);
	if (*sp != '.') {
		mins = secs;
		secs = 0;
	} else {
		sp++;
		mins = gp(-1);
	}
	hour = gp(-1);
	day = gp(L->tm_mday);
	month = gp(L->tm_mon+1);
	year = gp(L->tm_year);
	if (*sp)
		return (1);
	if (month < 1 || month > 12 ||
	    day < 1 || day > 31 ||
	    mins < 0 || mins > 59 ||
	    secs < 0 || secs > 59)
		return (1);
	if (hour == 24) {
		hour = 0;
		day++;
	}
	if (hour < 0 || hour > 23)
		return (1);
	tv.tv_sec = 0;
	year += 1900;
	for (i = 1970; i < year; i++)
		tv.tv_sec += dysize(i);
	/* Leap year */
	if (dysize(year) == 366 && month >= 3)
		tv.tv_sec++;
	while (--month)
		tv.tv_sec += dmsize[month-1];
	tv.tv_sec += day-1;
	tv.tv_sec = 24*tv.tv_sec + hour;
	tv.tv_sec = 60*tv.tv_sec + mins;
	tv.tv_sec = 60*tv.tv_sec + secs;
	return (0);
}

gp(dfault)
{
	register int c, d;

	if (*sp == 0)
		return (dfault);
	c = (*sp++) - '0';
	d = (*sp ? (*sp++) - '0' : 0);
	if (c < 0 || c > 9 || d < 0 || d > 9)
		return (-1);
	return (c+10*d);
}

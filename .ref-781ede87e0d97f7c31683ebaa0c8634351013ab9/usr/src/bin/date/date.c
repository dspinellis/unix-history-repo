/*
 * Copyright (c) 1985, 1987, 1988 The Regents of the University of California.
 * All rights reserved.
 *
 * %sccs.include.redist.c%
 */

#ifndef lint
char copyright[] =
"@(#) Copyright (c) 1985, 1987, 1988 The Regents of the University of California.\n\
 All rights reserved.\n";
#endif /* not lint */

#ifndef lint
static char sccsid[] = "@(#)date.c	4.28 (Berkeley) %G%";
#endif /* not lint */

#include <sys/param.h>
#include <sys/time.h>
#include <sys/file.h>
#include <syslog.h>
#include <stdio.h>
#include <ctype.h>
#include <string.h>
#include <stdlib.h>

time_t tval;
int retval, nflag;

main(argc, argv)
	int argc;
	char **argv;
{
	extern int optind;
	extern char *optarg;
	struct timezone tz;
	int ch;
	char *format, buf[1024];

	tz.tz_dsttime = tz.tz_minuteswest = 0;
	while ((ch = getopt(argc, argv, "d:nut:")) != EOF)
		switch((char)ch) {
		case 'd':		/* daylight savings time */
			tz.tz_dsttime = atoi(optarg) ? 1 : 0;
			break;
		case 'n':		/* don't set network */
			nflag = 1;
			break;
		case 'u':		/* do everything in GMT */
			(void)setenv("TZ", "GMT", 1);
			break;
		case 't':		/* minutes west of GMT */
					/* error check; don't allow "PST" */
			if (isdigit(*optarg)) {
				tz.tz_minuteswest = atoi(optarg);
				break;
			}
			/* FALLTHROUGH */
		default:
			usage();
		}
	argc -= optind;
	argv += optind;

	/*
	 * If -d or -t, set the timezone or daylight savings time; this
	 * doesn't belong here, there kernel should not know about either.
	 */
	if ((tz.tz_minuteswest || tz.tz_dsttime) &&
	    settimeofday((struct timeval *)NULL, &tz)) {
		perror("date: settimeofday");
		exit(1);
	}

	if (time(&tval) == (time_t)-1) {
		perror("date: time");
		exit(1);
	}

	format = "%a %b %e %H:%M:%S %Z %Y\n";

	/* allow the operands in any order */
	if (*argv && **argv == '+') {
		format = *argv + 1;
		++argv;
	}

	if (*argv) {
		setthetime(*argv);
		++argv;
	}

	if (*argv && **argv == '+')
		format = *argv + 1;

	(void)strftime(buf, sizeof(buf), format, localtime(&tval));
	(void)printf("%s", buf);
	exit(retval);
}

#define	ATOI2(ar)	((ar)[0] - '0') * 10 + ((ar)[1] - '0'); (ar) += 2;
setthetime(p)
	register char *p;
{
	register struct tm *lt;
	struct timeval tv;
	int dot;
	char *t;

	for (t = p, dot = 0; *t; ++t)
		if (!isdigit(*t) && (*t != '.' || dot++))
			badformat();

	lt = localtime(&tval);

	if (t = index(p, '.')) {		/* .ss */
		*t++ = '\0';
		if (lt->tm_sec > 61)
			badformat();
	} else
		lt->tm_sec = 0;

	for (t = p; *t; ++t)
		if (!isdigit(*t))
			badformat();

	switch (strlen(p)) {
	case 10:				/* yy */
		lt->tm_year = ATOI2(p);
		if (lt->tm_year < 69)		/* hack for 2000 ;-} */
			lt->tm_year += 100;
		/* FALLTHROUGH */
	case 8:					/* mm */
		lt->tm_mon = ATOI2(p);
		if (lt->tm_mon > 12)
			badformat();
		/* FALLTHROUGH */
	case 6:					/* dd */
		lt->tm_mday = ATOI2(p);
		if (lt->tm_mday > 31)
			badformat();
		/* FALLTHROUGH */
	case 4:					/* hh */
		lt->tm_hour = ATOI2(p);
		if (lt->tm_hour > 23)
			badformat();
		/* FALLTHROUGH */
	case 2:					/* mm */
		lt->tm_min = ATOI2(p);
		if (lt->tm_min > 59)
			badformat();
		break;
	default:
		badformat();
	}

	/* convert broken-down time to GMT clock time */
	if ((tval = mktime(lt)) == -1)
		badformat();

	if (!(p = getlogin()))			/* single-user or no tty */
		p = "root";
	syslog(LOG_AUTH | LOG_NOTICE, "date set by %s", p);

	/* set the time */
	if (nflag || netsettime(tval)) {
		logwtmp("|", "date", "");
		tv.tv_sec = tval;
		tv.tv_usec = 0;
		if (settimeofday(&tv, (struct timezone *)NULL)) {
			perror("date: settimeofday");
			exit(1);
		}
		logwtmp("{", "date", "");
	}
}

badformat()
{
	(void)fprintf(stderr, "date: illegal time format.\n");
	usage();
}

usage()
{
	(void)fprintf(stderr,
"usage: date [-nu] [-d dst] [-t west] [+format] [yy[mm[dd[hh]]]]mm[.ss]]\n");
	exit(1);
}

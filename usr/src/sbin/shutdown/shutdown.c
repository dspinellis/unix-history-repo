/*
 * Copyright (c) 1988 Regents of the University of California.
 * All rights reserved.
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
 * WARRANTIES OF MERCHANTIBILITY AND FITNESS FOR A PARTICULAR PURPOSE.
 */

#ifndef lint
char copyright[] =
"@(#) Copyright (c) 1988 Regents of the University of California.\n\
 All rights reserved.\n";
#endif /* not lint */

#ifndef lint
static char sccsid[] = "@(#)shutdown.c	5.9 (Berkeley) %G%";
#endif /* not lint */

#include <sys/param.h>
#include <sys/time.h>
#include <sys/file.h>
#include <sys/resource.h>
#include <sys/syslog.h>
#include <signal.h>
#include <setjmp.h>
#include <tzfile.h>
#include <pwd.h>
#include <stdio.h>
#include <ctype.h>

#define	REBOOT		"/etc/reboot"
#define	HALT		"/etc/halt"

#ifdef DEBUG
#define	NOLOGIN		"./nologin"
#define	FASTBOOT	"./fastboot"
#else
#define	NOLOGIN		"/etc/nologin"
#define	FASTBOOT	"/fastboot"
#endif

#define	H		*60*60
#define	M		*60
#define	S		*1
#define	NOLOG_TIME	5*60
struct interval {
	int timeleft, timetowait;
} tlist[] = {
	10 H,  5 H,	 5 H,  3 H,	 2 H,  1 H,	1 H, 30 M,
	30 M, 10 M,	20 M, 10 M,	10 M,  5 M,	5 M,  3 M,
	 2 M,  1 M,	 1 M, 30 S,	30 S, 30 S,
	 0, 0,
}, *tp = tlist;
#undef H
#undef M
#undef S

static time_t offset, shuttime;
static int dofast, dohalt, doreboot, killflg, mbuflen;
static char *nosync, *whom, mbuf[BUFSIZ];

main(argc, argv)
	int argc;
	char **argv;
{
	extern int optind;
	register char *p, *endp;
	int arglen, ch, len, readstdin;
	struct passwd *pw, *getpwuid();
	char *strcat(), *getlogin();
	uid_t geteuid();

#ifndef DEBUG
	if (geteuid()) {
		fprintf(stderr, "shutdown: NOT super-user\n");
		exit(1);
	}
#endif
	nosync = NULL;
	readstdin = 0;
	while ((ch = getopt(argc, argv, "-fhknr")) != EOF)
		switch((char)ch) {
		case '-':
			readstdin = 1;
			break;
		case 'f':
			dofast = 1;
			break;
		case 'h':
			dohalt = 1;
			break;
		case 'k':
			killflg = 1;
			break;
		case 'n':
			nosync = "-n";
			break;
		case 'r':
			doreboot = 1;
			break;
		case '?':
		default:
			usage();
		}
	argc -= optind;
	argv += optind;

	if (argc < 1)
		usage();

	if (dofast && nosync) {
		fprintf(stderr,
		    "shutdown: incompatible switches -f and -n.\n");
		usage();
	}
	if (doreboot && dohalt) {
		fprintf(stderr, 
		    "shutdown: incompatible switches -h and -r.\n");
		usage();
	}
	getoffset(*argv++);

	if (*argv) {
		for (p = mbuf, len = sizeof(mbuf); *argv; ++argv) {
			arglen = strlen(*argv);
			if ((len -= arglen) <= 2)
				break;
			if (p != mbuf)
				*p++ = ' ';
			bcopy(*argv, p, arglen);
			p += arglen;
		}
		*p = '\n';
		*++p = '\0';
	}

	if (readstdin) {
		p = mbuf;
		endp = mbuf + sizeof(mbuf) - 2;
		for (;;) {
			if (!fgets(p, endp - p + 1, stdin))
				break;
			for (; *p &&  p < endp; ++p);
			if (p == endp) {
				*p = '\n';
				*++p = '\0';
				break;
			}
		}
	}
	mbuflen = strlen(mbuf);

	if (offset)
		printf("Shutdown at %.24s.", ctime(&shuttime));
	else
		printf("Shutdown NOW!\n");

	if (!(whom = getlogin()))
		whom = (pw = getpwuid(getuid())) ? pw->pw_name : "???";

#ifdef DEBUG
	(void)putc('\n', stdout);
#else
	(void)setpriority(PRIO_PROCESS, 0, PRIO_MIN);
	{
		int forkpid;

		forkpid = fork();
		if (forkpid == -1) {
			perror("shutdown: fork");
			exit(1);
		}
		if (forkpid) {
			printf("shutdown: [pid %d]\n", forkpid);
			exit(0);
		}
	}
#endif
	openlog("shutdown", LOG_CONS, LOG_AUTH);
	loop();
	/*NOTREACHED*/
}

#define	WALL_CMD	"/bin/wall"

loop()
{
	u_int sltime;
	int logged;

	if (offset <= NOLOG_TIME) {
		logged = 1;
		nolog();
	}
	else
		logged = 0;
	tp = tlist;
	if (tp->timeleft < offset)
		(void)sleep((u_int)(offset - tp->timeleft));
	else {
		while (offset < tp->timeleft)
			++tp;
		/*
		 * warn now, if going to sleep more than a fifth of
		 * the next wait time.
		 */
		if (sltime = offset - tp->timeleft) {
			if (sltime > tp->timetowait / 5)
				warn();
			(void)sleep(sltime);
		}
	}
	for (;; ++tp) {
		warn();
		if (!logged && tp->timeleft <= NOLOG_TIME) {
			logged = 1;
			nolog();
		}
		(void)sleep((u_int)tp->timetowait);
		if (!tp->timeleft)
			break;
	}
	die_you_gravy_sucking_pig_dog();
}

static jmp_buf alarmbuf;

warn()
{
	static int first;
	static char hostname[MAXHOSTNAMELEN];
	FILE *pf;
	char *ctime();
	int timeout();

	if (!first++) {
		(void)signal(SIGALRM, timeout);
		(void)gethostname(hostname, sizeof(hostname));
	}

	if (!(pf = popen(WALL_CMD, "w"))) {
		syslog(LOG_ERR, "shutdown: can't find %s: %m", WALL_CMD);
		return;
	}

	fprintf(pf, "*** %sSystem shutdown message ***\n",
	    tp->timeleft ? "": "FINAL ");

	if (tp->timeleft > 10*60)
		fprintf(pf, "System going down at %5.5s\n\n",
		    ctime(&shuttime) + 11);
	else if (tp->timeleft > 59)
		fprintf(pf, "System going down in %d minute%s\n\n",
		    tp->timeleft / 60, (tp->timeleft > 60) ? "s" : "");
	else if (tp->timeleft)
		fprintf(pf, "System going down in 30 seconds\n\n");
	else
		fprintf(pf, "System going down IMMEDIATELY\n\n");

	if (mbuflen)
		(void)fwrite(mbuf, sizeof(*mbuf), mbuflen, pf);

	/*
	 * play some games, just in case wall doesn't come back
	 * probably unecessary, given that wall is careful.
	 */
	if (!setjmp(alarmbuf)) {
		(void)alarm((u_int)30);
		(void)pclose(pf);
		(void)alarm((u_int)0);
	}
}

timeout()
{
	longjmp(alarmbuf, 1);
}

die_you_gravy_sucking_pig_dog()
{
	syslog(LOG_NOTICE, "%s by %s: %s",
	    doreboot ? "reboot" : dohalt ? "halt" : "shutdown", whom, mbuf);
	(void)sleep(2);

	printf("\r\nSystem shutdown time has arrived\007\007\r\n");
	if (killflg) {
		printf("\rbut you'll have to do it yourself\r\n");
		finish();
	}
	if (dofast)
		doitfast();
#ifdef DEBUG
	if (doreboot)
		printf("reboot");
	else if (dohalt)
		printf("halt");
	if (nosync)
		printf(" no sync");
	if (dofast)
		printf(" no fsck");
	printf("\nkill -HUP 1\n");
#else
	if (doreboot) {
		execle(REBOOT, "reboot", "-l", nosync, 0);
		syslog(LOG_ERR, "shutdown: can't exec %s: %m.", REBOOT);
		perror("shutdown");
	}
	else if (dohalt) {
		execle(HALT, "halt", "-l", nosync, 0);
		syslog(LOG_ERR, "shutdown: can't exec %s: %m.", HALT);
		perror("shutdown");
	}
	(void)kill(1, SIGTERM);		/* to single user */
#endif
	finish();
}

#define	ATOI2(p)	(p[0] - '0') * 10 + (p[1] - '0'); p += 2;
static int dmsize[] =
	{ -1, 31, 28, 31, 30, 31, 30, 31, 31, 30, 31, 30, 31 };

getoffset(timearg)
	register char *timearg;
{
	register struct tm *lt;
	register char *p;
	time_t now, time();
	int year, month, day, hour, min;

	if (!strcasecmp(timearg, "now")) {		/* now */
		offset = 0;
		return;
	}

	(void)time(&now);
	if (*timearg == '+') {				/* +minutes */
		if (!isdigit(*++timearg))
			goto badtime;
		min = atoi(timearg);
		offset = min * 60;
		shuttime = now + offset;
		return;
	}

	/* handle hh:mm by getting rid of the colon */
	for (p = timearg; *p; ++p)
		if (!isascii(*p) || !isdigit(*p))
			if (*p == ':' && strlen(p) == 3) {
				p[0] = p[1];
				p[1] = p[2];
				p[2] = '\0';
			}
			else
				goto badtime;

	unsetenv("TZ");					/* OUR timezone */
	lt = localtime(&now);				/* [yymmdd]hhmm */
	year = lt->tm_year;
	month = lt->tm_mon + 1;
	day = lt->tm_mday;

	switch(strlen(timearg)) {
	case 10:
		year = ATOI2(timearg);
		/* FALLTHROUGH */
	case 8:
		month = ATOI2(timearg);
		/* FALLTHROUGH */
	case 6:
		day = ATOI2(timearg);
		/* FALLTHROUGH */
	case 4:
		hour = ATOI2(timearg);
		min = ATOI2(timearg);
		if (month < 1 || month > 12 || day < 1 || day > 31 ||
		    hour < 0 || hour > 23 || min < 0 || min > 59)
			goto badtime;
		shuttime = 0;
		year += TM_YEAR_BASE;
		if (isleap(year) && month > 2)
			++shuttime;
		for (--year; year >= EPOCH_YEAR; --year)
			shuttime += isleap(year) ?
			    DAYS_PER_LYEAR : DAYS_PER_NYEAR;
		while (--month)
			shuttime += dmsize[month];
		shuttime += day - 1;
		shuttime = HOURS_PER_DAY * shuttime + hour;
		shuttime = MINS_PER_HOUR * shuttime + min;
		shuttime *= SECS_PER_MIN;
		shuttime -= lt->tm_gmtoff;
		if ((offset = shuttime - now) >= 0)
			break;
		/* FALLTHROUGH */
	default:
badtime:	fprintf(stderr,
		    "shutdown: bad time format, or already past.\n");
		exit(1);
	}
}

#define	FSMSG	"fastboot file for fsck\n"
doitfast()
{
	int fastfd;

	if ((fastfd = open(FASTBOOT, O_WRONLY|O_CREAT|O_TRUNC, 0664)) >= 0) {
		(void)write(fastfd, FSMSG, sizeof(FSMSG) - 1);
		(void)close(fastfd);
	}
}

#define	NOMSG	"\n\nNO LOGINS: System going down at "
nolog()
{
	int logfd, finish();
	char *ct, *ctime();

	(void)unlink(NOLOGIN);		/* in case linked to another file */
	(void)signal(SIGINT, finish);
	(void)signal(SIGHUP, finish);
	(void)signal(SIGQUIT, finish);
	(void)signal(SIGTERM, finish);
	if ((logfd = open(NOLOGIN, O_WRONLY|O_CREAT|O_TRUNC, 0664)) >= 0) {
		(void)write(logfd, NOMSG, sizeof(NOMSG) - 1);
		ct = ctime(&shuttime);
		(void)write(logfd, ct + 11, 5);
		(void)write(logfd, "\n\n", 2);
		(void)write(logfd, mbuf, strlen(mbuf));
		(void)close(logfd);
	}
}

finish()
{
	(void)unlink(NOLOGIN);
	exit(0);
}

usage()
{
	fprintf(stderr, "usage: shutdown [-fhknr] shutdowntime [ message ]\n");
	exit(1);
}

/*
 * Copyright (c) 1983 Regents of the University of California.
 * All rights reserved.  The Berkeley software License Agreement
 * specifies the terms and conditions for redistribution.
 */

#ifndef lint
char copyright[] =
"@(#) Copyright (c) 1983 Regents of the University of California.\n\
 All rights reserved.\n";
#endif not lint

#ifndef lint
static char sccsid[] = "@(#)shutdown.c	5.1 (Berkeley) %G%";
#endif not lint

#include <stdio.h>
#include <ctype.h>
#include <signal.h>
#include <setjmp.h>
#include <utmp.h>
#include <sys/time.h>
#include <sys/resource.h>
#include <sys/types.h>
/*
 *	/etc/shutdown when [messages]
 *
 *	allow super users to tell users and remind users
 *	of iminent shutdown of unix
 *	and shut it down automatically
 *	and even reboot or halt the machine if they desire
 */
#ifdef DEBUG
#define LOGFILE "shutdown.log"
#else
#define LOGFILE "/usr/adm/shutdownlog"
#endif
#define	REBOOT	"/etc/reboot"
#define	HALT	"/etc/halt"
#define MAXINTS 20
#define	HOURS	*3600
#define MINUTES	*60
#define SECONDS
#define NLOG		20		/* no of args possible for message */
#define	NOLOGTIME	5 MINUTES
#define IGNOREUSER	"sleeper"

char	hostname[32];

int	timeout();
time_t	getsdt();

extern	char *ctime();
extern	struct tm *localtime();

struct	utmp utmp;
int	sint;
int	stogo;
char	tpath[] =	"/dev/";
int	nlflag = 1;		/* nolog yet to be done */
int	killflg = 1;
int	reboot = 0;
int	halt = 0;
char	term[sizeof tpath + sizeof utmp.ut_line];
char	tbuf[BUFSIZ];
char	nolog1[] = "\n\nNO LOGINS: System going down at %5.5s\n\n";
char	*nolog2[NLOG+1];
#ifdef	DEBUG
char	nologin[] = "nologin";
#else
char	nologin[] = "/etc/nologin";
#endif
time_t	nowtime;
jmp_buf	alarmbuf;

struct interval {
	int stogo;
	int sint;
} interval[] = {
	4 HOURS,	1 HOURS,
	2 HOURS,	30 MINUTES,
	1 HOURS,	15 MINUTES,
	30 MINUTES,	10 MINUTES,
	15 MINUTES,	5 MINUTES,
	10 MINUTES,	5 MINUTES,
	5 MINUTES,	3 MINUTES,
	2 MINUTES,	1 MINUTES,
	1 MINUTES,	30 SECONDS,
	0 SECONDS,	0 SECONDS
};

char *shutter, *getlogin();

main(argc,argv)
	int argc;
	char **argv;
{
	register i, ufd;
	register char **mess, *f;
	char *ts;
	time_t sdt;
	int h, m;
	int first;
	FILE *termf;

	shutter = getlogin();
	gethostname(hostname, sizeof (hostname));
	argc--, argv++;
	while (argc > 0 && (f = argv[0], *f++ == '-')) {
		while (i = *f++) switch (i) {
		case 'k':
			killflg = 0;
			continue;
		case 'r':
			reboot = 1;
			continue;
		case 'h':
			halt = 1;
			continue;
		default:
			fprintf(stderr, "shutdown: '%c' - unknown flag\n", i);
			exit(1);
		}
		argc--, argv++;
	}
	if (argc < 1) {
		printf("Usage: %s [ -krh ] shutdowntime [ message ]\n",
		    argv[0]);
		finish();
	}
	if (geteuid()) {
		fprintf(stderr, "NOT super-user\n");
		finish();
	}
	nowtime = time((time_t *)0);
	sdt = getsdt(argv[0]);
	argc--, argv++;
	i = 0;
	while (argc-- > 0)
		if (i < NLOG)
			nolog2[i++] = *argv++;
	nolog2[i] = NULL;
	m = ((stogo = sdt - nowtime) + 30)/60;
	h = m/60; 
	m %= 60;
	ts = ctime(&sdt);
	printf("Shutdown at %5.5s (in ", ts+11);
	if (h > 0)
		printf("%d hour%s ", h, h != 1 ? "s" : "");
	printf("%d minute%s) ", m, m != 1 ? "s" : "");
#ifndef DEBUG
	signal(SIGHUP, SIG_IGN);
	signal(SIGQUIT, SIG_IGN);
	signal(SIGINT, SIG_IGN);
#endif
	signal(SIGTTOU, SIG_IGN);
	signal(SIGTERM, finish);
	signal(SIGALRM, timeout);
	setpriority(PRIO_PROCESS, 0, PRIO_MIN);
	fflush(stdout);
#ifndef DEBUG
	if (i = fork()) {
		printf("[pid %d]\n", i);
		exit(0);
	}
#else
	putc('\n', stdout);
#endif
	sint = 1 HOURS;
	f = "";
	ufd = open("/etc/utmp",0);
	if (ufd < 0) {
		perror("shutdown: /etc/utmp");
		exit(1);
	}
	first = 1;
	for (;;) {
		for (i = 0; stogo <= interval[i].stogo && interval[i].sint; i++)
			sint = interval[i].sint;
		if (stogo > 0 && (stogo-sint) < interval[i].stogo)
			sint = stogo - interval[i].stogo;
		if (stogo <= NOLOGTIME && nlflag) {
			nlflag = 0;
			nolog(sdt);
		}
		if (sint >= stogo || sint == 0)
			f = "FINAL ";
		nowtime = time((time_t *) 0);
		lseek(ufd, 0L, 0);
		while (read(ufd,&utmp,sizeof utmp)==sizeof utmp)
		if (utmp.ut_name[0] &&
		    strncmp(utmp.ut_name, IGNOREUSER, sizeof(utmp.ut_name))) {
			if (setjmp(alarmbuf))
				continue;
			strcpy(term, tpath);
			strncat(term, utmp.ut_line, sizeof utmp.ut_line);
			alarm(3);
#ifdef DEBUG
			if ((termf = stdout) != NULL)
#else
			if ((termf = fopen(term, "w")) != NULL)
#endif
			{
				alarm(0);
				setbuf(termf, tbuf);
				fprintf(termf, "\n\r\n");
				warn(termf, sdt, nowtime, f);
				if (first || sdt - nowtime > 1 MINUTES) {
					if (*nolog2)
						fprintf(termf, "\t...");
					for (mess = nolog2; *mess; mess++)
						fprintf(termf, " %s", *mess);
				}
				fputc('\r', termf);
				fputc('\n', termf);
				alarm(5);
#ifdef DEBUG
				fflush(termf);
#else
				fclose(termf);
#endif
				alarm(0);
			}
		}
		if (stogo <= 0) {
	printf("\n\007\007System shutdown time has arrived\007\007\n");
			log_entry(sdt);
			unlink(nologin);
			if (!killflg) {
				printf("but you'll have to do it yourself\n");
				finish();
			}
#ifndef DEBUG
			kill(-1, SIGTERM);	/* terminate everyone */
			sleep(5);		/* & wait while they die */
			if (reboot)
				execle(REBOOT, "reboot", 0, 0);
			if (halt)
				execle(HALT, "halt", 0, 0);
			kill(1, SIGTERM);	/* sync */
			kill(1, SIGTERM);	/* sync */
			sleep(20);
#else
			printf("EXTERMINATE EXTERMINATE\n");
#endif
			finish();
		}
		stogo = sdt - time((time_t *) 0);
		if (stogo > 0 && sint > 0)
			sleep(sint<stogo ? sint : stogo);
		stogo -= sint;
		first = 0;
	}
}

time_t
getsdt(s)
	register char *s;
{
	time_t t, t1, tim;
	register char c;
	struct tm *lt;

	if (strcmp(s, "now") == 0)
		return(nowtime);
	if (*s == '+') {
		++s; 
		t = 0;
		for (;;) {
			c = *s++;
			if (!isdigit(c))
				break;
			t = t * 10 + c - '0';
		}
		if (t <= 0)
			t = 5;
		t *= 60;
		tim = time((time_t *) 0) + t;
		return(tim);
	}
	t = 0;
	while (strlen(s) > 2 && isdigit(*s))
		t = t * 10 + *s++ - '0';
	if (*s == ':')
		s++;
	if (t > 23)
		goto badform;
	tim = t*60;
	t = 0;
	while (isdigit(*s))
		t = t * 10 + *s++ - '0';
	if (t > 59)
		goto badform;
	tim += t; 
	tim *= 60;
	t1 = time((time_t *) 0);
	lt = localtime(&t1);
	t = lt->tm_sec + lt->tm_min*60 + lt->tm_hour*3600;
	if (tim < t || tim >= (24*3600)) {
		/* before now or after midnight */
		printf("That must be tomorrow\nCan't you wait till then?\n");
		finish();
	}
	return (t1 + tim - t);
badform:
	printf("Bad time format\n");
	finish();
}

warn(term, sdt, now, type)
	FILE *term;
	time_t sdt, now;
	char *type;
{
	char *ts;
	register delay = sdt - now;

	if (delay > 8)
		while (delay % 5)
			delay++;

	if (shutter)
		fprintf(term,
	    "\007\007\t*** %sSystem shutdown message from %s@%s ***\r\n\n",
		    type, shutter, hostname);
	else
		fprintf(term,
		    "\007\007\t*** %sSystem shutdown message (%s) ***\r\n\n",
		    type, hostname);

	ts = ctime(&sdt);
	if (delay > 10 MINUTES)
		fprintf(term, "System going down at %5.5s\r\n", ts+11);
	else if (delay > 95 SECONDS) {
		fprintf(term, "System going down in %d minute%s\r\n",
		    (delay+30)/60, (delay+30)/60 != 1 ? "s" : "");
	} else if (delay > 0) {
		fprintf(term, "System going down in %d second%s\r\n",
		    delay, delay != 1 ? "s" : "");
	} else
		fprintf(term, "System going down IMMEDIATELY\r\n");
}

nolog(sdt)
	time_t sdt;
{
	FILE *nologf;
	register char **mess;

	unlink(nologin);			/* in case linked to std file */
	if ((nologf = fopen(nologin, "w")) != NULL) {
		fprintf(nologf, nolog1, (ctime(&sdt)) + 11);
		putc('\t', nologf);
		for (mess = nolog2; *mess; mess++)
			fprintf(nologf, " %s", *mess);
		putc('\n', nologf);
		fclose(nologf);
	}
}

finish()
{
	signal(SIGTERM, SIG_IGN);
	unlink(nologin);
	exit(0);
}

timeout()
{
	longjmp(alarmbuf, 1);
}

/*
 * make an entry in the shutdown log
 */

char *days[] = {
	"Sun", "Mon", "Tue", "Wed", "Thu", "Fri", "Sat"
};

char *months[] = {
	"Jan", "Feb", "Mar", "Apr", "May", "Jun", "Jul", "Aug", "Sep",
	"Oct", "Nov", "Dec"
};

log_entry(now)
	time_t now;
{
	register FILE *fp;
	register char **mess;
	struct tm *tm, *localtime();

	tm = localtime(&now);
	fp = fopen(LOGFILE, "a");
	if (fp == NULL) {
		printf("Shutdown: log entry failed\n");
		return;
	}
	fseek(fp, 0L, 2);
	fprintf(fp, "%02d:%02d  %s %s %2d, %4d.  Shutdown:", tm->tm_hour,
		tm->tm_min, days[tm->tm_wday], months[tm->tm_mon],
		tm->tm_mday, tm->tm_year + 1900);
	for (mess = nolog2; *mess; mess++)
		fprintf(fp, " %s", *mess);
	if (shutter)
		fprintf(fp, " (by %s!%s)", hostname, shutter);
	fputc('\n', fp);
	fclose(fp);
}

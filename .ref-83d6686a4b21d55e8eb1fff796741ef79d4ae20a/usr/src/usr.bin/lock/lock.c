/*
 * Copyright (c) 1980, 1987 Regents of the University of California.
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
"@(#) Copyright (c) 1980, 1987 Regents of the University of California.\n\
 All rights reserved.\n";
#endif /* not lint */

#ifndef lint
static char sccsid[] = "@(#)lock.c	5.10 (Berkeley) %G%";
#endif /* not lint */

/*
 * Lock a terminal up until the given key is entered, until the root
 * password is entered, or the given interval times out.
 *
 * Timeout interval is by default TIMEOUT, it can be changed with
 * an argument of the form -time where time is in minutes
 */

#include <sys/param.h>
#include <sys/stat.h>
#include <sys/time.h>
#include <sys/signal.h>
#include <sgtty.h>
#include <pwd.h>
#include <stdio.h>
#include <ctype.h>
#include <strings.h>

#define	TIMEOUT	15

int	quit(), bye(), hi();

struct timeval	timeout;
struct timeval	zerotime;
struct sgttyb	tty, ntty;
long	nexttime;			/* keep the timeout time */

/*ARGSUSED*/
main(argc, argv)
	int argc;
	char **argv;
{
	extern char *optarg;
	extern int errno, optind;
	struct passwd *pw;
	struct timeval timval;
	struct itimerval ntimer, otimer;
	struct tm *timp;
	int ch, sectimeout, usemine;
	char *ap, *mypw, *ttynam, *tzn;
	char hostname[MAXHOSTNAMELEN], s[BUFSIZ], s1[BUFSIZ];
	char *crypt(), *ttyname();

	sectimeout = TIMEOUT;
	mypw = NULL;
	usemine = 0;
	while ((ch = getopt(argc, argv, "pt:")) != EOF)
		switch((char)ch) {
		case 't':
			if ((sectimeout = atoi(optarg)) <= 0)
				exit(1);
			break;
		case 'p':
			usemine = 1;
			if (!(pw = getpwuid(getuid()))) {
				fprintf(stderr, "lock: unknown uid %d.\n",
				    getuid());
				exit(1);
			}
			mypw = strdup(pw->pw_passwd);
			break;
		case '?':
		default:
			fprintf(stderr, "usage: lock [-p] [-t timeout]\n");
			exit(1);
	}
	timeout.tv_sec = sectimeout * 60;

	setuid(getuid());		/* discard privs */

	if (ioctl(0, TIOCGETP, &tty))	/* get information for header */
		exit(1);
	gethostname(hostname, sizeof(hostname));
	if (!(ttynam = ttyname(0))) {
		printf("lock: not a terminal?\n");
		exit(1);
	}
	if (gettimeofday(&timval, (struct timezone *)NULL)) {
		fprintf(stderr, "lock: gettimeofday: %s\n", strerror(errno));
		exit(1);
	}
	nexttime = timval.tv_sec + (sectimeout * 60);
	timp = localtime(&timval.tv_sec);
	ap = asctime(timp);
	tzn = timp->tm_zone;

	(void)signal(SIGINT, quit);
	(void)signal(SIGQUIT, quit);
	ntty = tty; ntty.sg_flags &= ~ECHO;
	(void)ioctl(0, TIOCSETP, &ntty);

	if (!mypw) {
		/* get key and check again */
		printf("Key: ");
		if (!fgets(s, sizeof(s), stdin) || *s == '\n')
			quit();
		printf("\nAgain: ");
		/*
		 * Don't need EOF test here, if we get EOF, then s1 != s
		 * and the right things will happen.
		 */
		(void)fgets(s1, sizeof(s1), stdin);
		putchar('\n');
		if (strcmp(s1, s)) {
			printf("\07lock: passwords didn't match.\n");
			ioctl(0, TIOCSETP, &tty);
			exit(1);
		}
		s[0] = NULL;
		mypw = s1;
	}

	/* set signal handlers */
	(void)signal(SIGINT, hi);
	(void)signal(SIGQUIT, hi);
	(void)signal(SIGTSTP, hi);
	(void)signal(SIGALRM, bye);

	ntimer.it_interval = zerotime;
	ntimer.it_value = timeout;
	setitimer(ITIMER_REAL, &ntimer, &otimer);

	/* header info */
	printf ("lock: %s on %s. timeout in %d minutes\ntime now is %.20s%s%s",
		ttynam, hostname, sectimeout, ap, tzn, ap + 19);

	for (;;) {
		printf("Key: ");
		if (!fgets(s, sizeof(s), stdin)) {
			clearerr(stdin);
			hi();
			continue;
		}
		if (usemine) {
			s[strlen(s) - 1] = '\0';
			if (!strcmp(mypw, crypt(s, mypw)))
				break;
		}
		else if (!strcmp(s, s1))
			break;
		printf("\07\n");
		if (ioctl(0, TIOCGETP, &ntty))
			exit(1);
	}
	quit();
}

static
hi()
{
	struct timeval timval;

	if (!gettimeofday(&timval, (struct timezone *)NULL))
	    printf("lock: type in the unlock key. timeout in %ld:%ld minutes\n",
	    (nexttime - timval.tv_sec) / 60, (nexttime - timval.tv_sec) % 60);
}

static
quit()
{
	putchar('\n');
	(void)ioctl(0, TIOCSETP, &tty);
	exit(0);
}

static
bye()
{
	(void)ioctl(0, TIOCSETP, &tty);
	printf("lock: timeout\n");
	exit(1);
}

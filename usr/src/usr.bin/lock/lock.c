/*
 * Copyright (c) 1980 Regents of the University of California.
 * All rights reserved.  The Berkeley software License Agreement
 * specifies the terms and conditions for redistribution.
 */

#ifndef lint
char copyright[] =
"@(#) Copyright (c) 1980 Regents of the University of California.\n\
 All rights reserved.\n";
#endif not lint

#ifndef lint
static char sccsid[] = "@(#)lock.c	5.2 (Berkeley) %G%";
#endif not lint

/*
 * Lock a terminal up until the given key is entered,
 * or until the root password is entered,
 * or the given interval times out.
 *
 * Timeout interval is by default TIMEOUT, it can be changed with
 * an argument of the form -time where time is in minutes
 */

#include <sys/param.h>
#include <sys/stat.h>
#include <sys/time.h>
#include <sys/signal.h>
#include <pwd.h>
#include <sgtty.h>
#include <stdio.h>

#define	TIMEOUT 15

int	quit(), bye(), hi();

struct timeval	timeout;
struct timeval	zerotime;
struct sgttyb	tty, ntty;
long	nexttime;		/* keep the timeout time */

main(argc,argv)
	int	argc;
	char	**argv;
{
	struct passwd	*pwd;
	struct timeval	timval;
	struct itimerval	ntimer, otimer;
	struct tm	*timp;
	int	sectimeout = TIMEOUT;
	char	*ttynam, *ap, *tzn,
		hostname[MAXHOSTNAMELEN],
		s[BUFSIZ], s1[BUFSIZ],
		*crypt(), *index(), *ttyname();

	/* process argument */

	if (argc > 1 && (argv[1][0] != '-' ||
	    (sectimeout = atoi(argv[1] + 1)) <= 0)) {
		puts("Usage: lock [-timeout]");
		exit (1);
	}
	timeout.tv_sec = sectimeout * 60;

	/* get information for header */

	if (ioctl(0, TIOCGETP, &tty))
		exit(1);
	gethostname(hostname, sizeof(hostname));
	if (!(ttynam = ttyname(0))) {
		puts("lock: not a terminal?");
		exit (1);
	}
	if (gettimeofday(&timval, (struct timezone *)NULL)) {
		perror("gettimeofday");
		exit (1);
	}
	nexttime = timval.tv_sec + (sectimeout * 60);
	timp = localtime(&timval.tv_sec);
	ap = asctime(timp);
	tzn = timp->tm_zone;

	/* get key and check again */

	(void)signal(SIGINT, quit);
	(void)signal(SIGQUIT, quit);
	ntty = tty; ntty.sg_flags &= ~ECHO;
	(void)ioctl(0, TIOCSETP, &ntty);

	fputs("Key: ",stdout);
	if (!gets(s,sizeof(s)))
		quit();
	fputs("\nAgain: ",stdout);

	/*
	 * Don't need EOF test here, if we get EOF, then s1 != s
	 * and the right things will happen.
	 */
	(void)gets(s1,sizeof(s1));
	putchar('\n');
	if (strcmp(s1, s)) {
		putchar(07);
		ioctl(0, TIOCSETP, &tty);
		exit(1);
	}
	s[0] = NULL;

	/* Set signal handlers */

	(void)signal(SIGINT, hi);
	(void)signal(SIGQUIT, hi);
	(void)signal(SIGTSTP, hi);
	(void)signal(SIGALRM, bye);

	ntimer.it_interval = zerotime;
	ntimer.it_value = timeout;
	setitimer(ITIMER_REAL, &ntimer, &otimer);

	/* Header info */

	printf ("lock: %s on %s. timeout in %d minutes\ntime now is %.20s%s%s",
		ttynam,hostname,sectimeout,ap,tzn,ap+19);

	/* wait */

	for (pwd = getpwuid(0);;) {
		fputs("Key: ",stdout);
		if (!gets(s,sizeof(s))) {
			clearerr(stdin);
			hi();
			continue;
		}
		if (!strcmp(s1,s) || !pwd || !*pwd->pw_passwd ||
			!strcmp(pwd->pw_passwd,crypt(s,pwd->pw_passwd)))
			break;
		puts("\07");
		if (ioctl(0, TIOCGETP, &ntty))
			exit(1);
	}
	putchar('\n');
	quit();
}

/*
 *	tell the user we are waiting
 */

static
hi()
{
	struct timeval	timval;

	if (!gettimeofday(&timval, (struct timezone *)NULL))
	    printf("lock: type in the unlock key. timeout in %d minutes\n",
	    (nexttime - timval.tv_sec) / 60);
}

/*
 *	get out of here
 */
static
quit()
{
	(void)ioctl(0, TIOCSETP, &tty);
	exit (0);
}

static
bye()
{
	(void)ioctl(0, TIOCSETP, &tty);
	puts("lock: timeout");
	exit (1);
}

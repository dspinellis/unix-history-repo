/*
 * Copyright (c) 1980 Regents of the University of California.
 * All rights reserved.  The Berkeley software License Agreement
 * specifies the terms and conditions for redistribution.
 */

#ifndef lint
char copyright[] =
"@(#) Copyright (c) 1980 Regents of the University of California.\n\
 All rights reserved.\n";
#endif /* !lint */

#ifndef lint
static char sccsid[] = "@(#)lock.c	5.4 (Berkeley) %G%";
#endif /* !lint */

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
#include <pwd.h>
#include <sgtty.h>
#include <stdio.h>
#include <ctype.h>

#define	TIMEOUT	15
#define	YES	1
#define	NO	0

int	quit(), bye(), hi();

struct timeval	timeout;
struct timeval	zerotime;
struct sgttyb	tty, ntty;
long	nexttime;			/* keep the timeout time */

/*ARGSUSED*/
main(argc, argv)
	int	argc;
	char	**argv;
{
	struct passwd	*root_pwd, *my_pwd;
	struct timeval	timval;
	struct itimerval	ntimer, otimer;
	struct tm	*timp;
	int	sectimeout = TIMEOUT,
		use_mine;
	char	*ttynam, *ap, *tzn,
		hostname[MAXHOSTNAMELEN], s[BUFSIZ], s1[BUFSIZ],
		*crypt(), *index(), *ttyname();

	use_mine = NO;
	for (++argv; *argv; ++argv) {
		if (argv[0][0] != '-')
			usage();
		if (argv[0][1] == 'p')
			use_mine = YES;
		else if (!isdigit(argv[0][1])) {
			fprintf(stderr, "lock: illegal option -- %c\n", argv[0][1]);
			usage();
		}
		else if ((sectimeout = atoi(*argv + 1)) <= 0)
			usage();
	}
	timeout.tv_sec = sectimeout * 60;

	/* get information for header */
	if (ioctl(0, TIOCGETP, &tty))
		exit(1);
	gethostname(hostname, sizeof(hostname));
	if (!(ttynam = ttyname(0))) {
		puts("lock: not a terminal?");
		exit(1);
	}
	if (gettimeofday(&timval, (struct timezone *)NULL)) {
		perror("gettimeofday");
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

	if (!use_mine) {
		/* get key and check again */
		fputs("Key: ", stdout);
		if (!gets(s, sizeof(s)))
			quit();
		fputs("\nAgain: ", stdout);
		/*
		 * Don't need EOF test here, if we get EOF, then s1 != s
		 * and the right things will happen.
		 */
		(void)gets(s1, sizeof(s1));
		putchar('\n');
		if (strcmp(s1, s)) {
			puts("\07lock: passwords didn't match.");
			ioctl(0, TIOCSETP, &tty);
			exit(1);
		}
		s[0] = NULL;
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

	/* wait */
	root_pwd = getpwuid(0);
	if (use_mine)
		my_pwd = getpwuid(getuid());
	for (;;) {
		fputs("Key: ", stdout);
		if (!gets(s, sizeof(s))) {
			clearerr(stdin);
			hi();
			continue;
		}
		if (use_mine) {
			if (!my_pwd || !*my_pwd->pw_passwd || !strcmp(my_pwd->pw_passwd, crypt(s, my_pwd->pw_passwd)))
				break;
		}
		else if (!strcmp(s1, s))
			break;
		if (!root_pwd || !*root_pwd->pw_passwd || !strcmp(root_pwd->pw_passwd, crypt(s, root_pwd->pw_passwd)))
			break;
		puts("\07");
		if (ioctl(0, TIOCGETP, &ntty))
			exit(1);
	}
	putchar('\n');
	quit();
}

static
hi()
{
	struct timeval	timval;

	if (!gettimeofday(&timval, (struct timezone *)NULL))
	    printf("lock: type in the unlock key. timeout in %ld:%ld minutes\n",
	    (nexttime - timval.tv_sec) / 60, (nexttime - timval.tv_sec) % 60);
}

static
quit()
{
	(void)ioctl(0, TIOCSETP, &tty);
	exit(0);
}

static
bye()
{
	(void)ioctl(0, TIOCSETP, &tty);
	puts("lock: timeout");
	exit(1);
}

static
usage()
{
	fputs("usage: lock [-p] [-timeout]\n", stderr);
	exit(1);
}

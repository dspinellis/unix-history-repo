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
static char sccsid[] = "@(#)lock.c	5.1 (Berkeley) %G%";
#endif not lint

/*
 * Lock a terminal up until the given key is entered,
 * or until the root password is entered,
 * or the given interval times out.
 *
 * Timeout interval is by default TIMEOUT, it can be changed with
 * an argument of the form -time where time is in minutes
 */

#include <pwd.h>
#include <stdio.h>
#include <sys/types.h>
#include <sys/stat.h>
#include <sys/time.h>
#include <signal.h>
#include <sgtty.h>

#define TIMEOUT 15

struct	passwd *pwd;
char	*crypt();
char	*getpass();
char	*index();
char	*ttyname();
char	*timezone();
char	*asctime();
struct	tm *localtime();

int	quit();
int	bye();
int	hi();

struct timeval	timeout	= {0, 0};
struct timeval	zerotime = {0, 0};
struct sgttyb	tty, ntty;
long	nexttime;		/* keep the timeout time */

main(argc, argv)
	int argc;
	char **argv;
{
	register int t;
	char	*ttynam;
	char	*ap;
	int	sectimeout = TIMEOUT;
	char	s[BUFSIZ], s1[BUFSIZ];
	char	hostname[32];
	char	*tzn;
	struct timeval	timval;
	struct itimerval	ntimer, otimer;
	struct timezone	timzone;
	struct tm	*timp;
	struct stat	statb;

	/* process arguments */

	if (argc > 1){
		if (argv[1][0] != '-')
			goto usage;
		if (sscanf(&(argv[1][1]), "%d", &sectimeout) != 1)
			goto usage;
	}
	timeout.tv_sec = sectimeout * 60;

	/* get information for header */

	if (ioctl(0, TIOCGETP, &tty))
		exit(1);
	pwd = getpwuid(0);
	gethostname(hostname, sizeof(hostname));
	if (!(ttynam = ttyname(0))){
		printf("lock: not a terminal?");
		exit (1);
	}
	gettimeofday(&timval, &timzone);
	nexttime = timval.tv_sec + (sectimeout * 60);
	timp = localtime(&timval.tv_sec);
	ap = asctime(timp);
	tzn = timezone(timzone.tz_minuteswest, timp->tm_isdst);

	/* get key and check again */

	signal(SIGINT, quit);
	signal(SIGQUIT, quit);
	ntty = tty; ntty.sg_flags &= ~ECHO;
	ioctl(0, TIOCSETN, &ntty);
	printf("Key: ");
	if (fgets(s, sizeof s, stdin) == NULL) {
		putchar('\n');
		quit();
	}
	printf("\nAgain: ");
	/*
	 * Don't need EOF test here, if we get EOF, then s1 != s
	 * and the right things will happen.
	 */
	(void) fgets(s1, sizeof s1, stdin);
	putchar('\n');
	if (strcmp(s1, s)) {
		putchar(07);
		stty(0, &tty);
		exit(1);
	}
	s[0] = 0;

	/* Set signal handlers */

	signal(SIGINT, hi);
	signal(SIGQUIT, hi);
	signal(SIGTSTP, hi);
	signal(SIGALRM, bye);
	ntimer.it_interval = zerotime;
	ntimer.it_value = timeout;
	setitimer(ITIMER_REAL, &ntimer, &otimer);

	/* Header info */

	printf ("lock: %s on %s. timeout in %d minutes\n",
		ttynam, hostname, sectimeout);
	printf("time now is %.20s", ap);
	if (tzn)
		printf("%s", tzn);
	printf("%s", ap+19);

	/* wait */

	for (;;) {
		printf("Key: ");
		if (fgets(s, sizeof s, stdin) == NULL) {
			clearerr(stdin);
			hi();
			continue;
		}
		if (strcmp(s1, s) == 0)
			break;
		if (pwd == (struct passwd *) 0 || pwd->pw_passwd[0] == '\0')
			break;
		ap = index(s, '\n');
		if (ap != NULL)
			*ap = '\0';
		if (strcmp(pwd->pw_passwd, crypt(s, pwd->pw_passwd)) == 0)
			break;
		printf("\07\n");
		if (ioctl(0, TIOCGETP, &ntty))
			exit(1);
	}
	ioctl(0, TIOCSETN, &tty);
	putchar('\n');
	exit (0);
usage:
	printf("Usage: lock [-timeout]\n");
	exit (1);
}

/*
 *	get out of here
 */

quit()
{
	ioctl(0, TIOCSETN, &tty);
	exit (0);
}

bye()
{
	ioctl(0, TIOCSETN, &tty);
	printf("lock: timeout\n");
	exit (1);
}

/*
 *	tell the user we are waiting
 */

hi()
{
	long	curtime;
	struct timeval	timval;
	struct timezone	timzone;

	gettimeofday(&timval, &timzone);
	curtime = timval.tv_sec;
	printf("lock: type in the unlock key. timeout in %d minutes\n",
		(nexttime-curtime)/60);
}

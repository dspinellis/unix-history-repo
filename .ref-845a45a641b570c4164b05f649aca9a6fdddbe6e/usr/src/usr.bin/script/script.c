/*
 * Copyright (c) 1980, 1992 Regents of the University of California.
 * All rights reserved.
 *
 * %sccs.include.redist.c%
 */

#ifndef lint
char copyright[] =
"@(#) Copyright (c) 1980, 1992 Regents of the University of California.\n\
 All rights reserved.\n";
#endif /* not lint */

#ifndef lint
static char sccsid[] = "@(#)script.c	5.15 (Berkeley) %G%";
#endif /* not lint */

#include <sys/types.h>
#include <sys/wait.h>
#include <sys/stat.h>
#include <sys/ioctl.h>
#include <sys/time.h>

#include <errno.h>
#include <fcntl.h>
#include <paths.h>
#include <signal.h>
#include <stdio.h>
#include <stdlib.h>
#include <string.h>
#include <termios.h>
#include <tzfile.h>
#include <unistd.h>

FILE	*fscript;
int	master, slave;
int	child, subchild;
int	outcc;
char	*fname;

struct	termios tt;

__dead	void done __P((void));
	void dooutput __P((void));
	void doshell __P((void));
	void err __P((const char *, ...));
	void fail __P((void));
	void finish __P((int));
	void scriptflush __P((int));

int
main(argc, argv)
	int argc;
	char *argv[];
{
	register int cc;
	struct termios rtt;
	struct winsize win;
	int aflg, ch;
	char ibuf[BUFSIZ];

	aflg = 0;
	while ((ch = getopt(argc, argv, "a")) != EOF)
		switch(ch) {
		case 'a':
			aflg = 1;
			break;
		case '?':
		default:
			(void)fprintf(stderr, "usage: script [-a] [file]\n");
			exit(1);
		}
	argc -= optind;
	argv += optind;

	if (argc > 0)
		fname = argv[0];
	else
		fname = "typescript";

	if ((fscript = fopen(fname, aflg ? "a" : "w")) == NULL)
		err("%s: %s", fname, strerror(errno));

	(void)tcgetattr(STDIN_FILENO, &tt);
	(void)ioctl(STDIN_FILENO, TIOCGWINSZ, &win);
	if (openpty(&master, &slave, NULL, &tt, &win) == -1)
		err("openpty: %s", strerror(errno));

	(void)printf("Script started, output file is %s\n", fname);
	rtt = tt;
	cfmakeraw(&rtt);
	rtt.c_lflag &= ~ECHO;
	(void)tcsetattr(STDIN_FILENO, TCSAFLUSH, &rtt);

	(void)signal(SIGCHLD, finish);
	child = fork();
	if (child < 0) {
		perror("fork");
		fail();
	}
	if (child == 0) {
		subchild = child = fork();
		if (child < 0) {
			perror("fork");
			fail();
		}
		if (child)
			dooutput();
		else
			doshell();
	}

	(void)fclose(fscript);
	while ((cc = read(STDIN_FILENO, ibuf, BUFSIZ)) > 0)
		(void)write(master, ibuf, cc);
	done();
}

void
finish(signo)
	int signo;
{
	register int die, pid;
	union wait status;

	die = 0;
	while ((pid = wait3((int *)&status, WNOHANG, 0)) > 0)
		if (pid == child)
			die = 1;

	if (die)
		done();
}

void
dooutput()
{
	struct itimerval value;
	register int cc;
	time_t tvec;
	char obuf[BUFSIZ];

	(void)close(STDIN_FILENO);
	tvec = time(NULL);
	(void)fprintf(fscript, "Script started on %s", ctime(&tvec));

	(void)signal(SIGALRM, scriptflush);
	value.it_interval.tv_sec = SECSPERMIN / 2;
	value.it_interval.tv_usec = 0;
	value.it_value = value.it_interval;
	(void)setitimer(ITIMER_REAL, &value, NULL);
	for (;;) {
		cc = read(master, obuf, sizeof (obuf));
		if (cc <= 0)
			break;
		(void)write(1, obuf, cc);
		(void)fwrite(obuf, 1, cc, fscript);
		outcc += cc;
	}
	done();
}

void
scriptflush(signo)
	int signo;
{
	if (outcc) {
		(void)fflush(fscript);
		outcc = 0;
	}
}

void
doshell()
{
	char *shell;

	shell = getenv("SHELL");
	if (shell == NULL)
		shell = _PATH_BSHELL;

	(void)close(master);
	(void)fclose(fscript);
	login_tty(slave);
	execl(shell, "sh", "-i", NULL);
	perror(shell);
	fail();
}

void
fail()
{

	(void)kill(0, SIGTERM);
	done();
}

void
done()
{
	time_t tvec;

	if (subchild) {
		tvec = time(NULL);
		(void)fprintf(fscript,"\nScript done on %s", ctime(&tvec));
		(void)fclose(fscript);
		(void)close(master);
	} else {
		(void)tcsetattr(STDIN_FILENO, TCSAFLUSH, &tt);
		(void)printf("Script done, output file is %s\n", fname);
	}
	exit(0);
}

#if __STDC__
#include <stdarg.h>
#else
#include <varargs.h>
#endif

void
#if __STDC__
err(const char *fmt, ...)
#else
err(fmt, va_alist)
	char *fmt;
	va_dcl
#endif
{
	va_list ap;
#if __STDC__
	va_start(ap, fmt);
#else
	va_start(ap);
#endif
	(void)fprintf(stderr, "script: ");
	(void)vfprintf(stderr, fmt, ap);
	va_end(ap);
	(void)fprintf(stderr, "\n");
	exit(1);
	/* NOTREACHED */
}

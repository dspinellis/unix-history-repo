/*
 * Copyright (c) 1987 Regents of the University of California.
 * All rights reserved.  The Berkeley software License Agreement
 * specifies the terms and conditions for redistribution.
 */

#ifndef lint
static char sccsid[] = "@(#)update.c	4.3 (Berkeley) 3/28/87";
#endif

/*
 * Update the file system every 30 seconds.
 * For cache benefit, open certain system directories.
 */

#include <sys/time.h>
#include <sys/file.h>
#include <sys/signal.h>
#include <syslog.h>
#include <stdio.h>

char *fillst[] = {
	"/bin",
	"/lib",
	"/usr",
	"/usr/bin",
	"/usr/lib",
	"/usr/ucb",
	0,
};

main()
{
	struct itimerval	value;
	register char	**f;
	extern int	sync();

	if (fork())
		exit(0);
	(void)close(0);
	(void)close(1);
	(void)close(2);
	for (f = fillst; *f; f++)
		(void)open(*f, O_RDONLY, 0);
	(void)signal(SIGALRM, sync);
	value.it_interval.tv_sec = 30;
	value.it_interval.tv_usec = 0;
	value.it_value = value.it_interval;
	if (setitimer(ITIMER_REAL, &value, (struct itimerval *)NULL)) {
		perror("update: setitimer");
		exit(1);
	}
	for (;;)
		pause();
	/*NOTREACHED*/
}

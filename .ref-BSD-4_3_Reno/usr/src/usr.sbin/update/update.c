/*
 * Copyright (c) 1987, 1990 Regents of the University of California.
 * All rights reserved.  The Berkeley software License Agreement
 * specifies the terms and conditions for redistribution.
 */

#ifndef lint
static char sccsid[] = "@(#)update.c	4.5 (Berkeley) 6/29/90";
#endif

/*
 * Update the file system every 30 seconds.
 * For cache benefit, open certain system directories.
 */

#include <sys/time.h>
#include <sys/file.h>
#include <sys/signal.h>
#include "pathnames.h"

main()
{
	struct itimerval value;
	register char **f;
	extern int sync();

	daemon(0, 0);
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

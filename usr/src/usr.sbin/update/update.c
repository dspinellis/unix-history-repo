/*-
 * Copyright (c) 1987, 1990 The Regents of the University of California.
 * All rights reserved.
 *
 * %sccs.include.redist.c%
 */

#ifndef lint
char copyright[] =
"@(#) Copyright (c) 1987, 1990 The Regents of the University of California.\n\
 All rights reserved.\n";
#endif /* not lint */

#ifndef lint
static char sccsid[] = "@(#)update.c	4.7 (Berkeley) %G%";
#endif /* not lint */

#include <sys/time.h>
#include <signal.h>
#include <fcntl.h>
#include <stdlib.h>
#include <stdio.h>
#include <unistd.h>
#include "pathnames.h"

main()
{
	struct itimerval value;
	void mysync();

	daemon(0, 0);

	(void)open(_PATH_BIN, O_RDONLY, 0);
	(void)open(_PATH_USR, O_RDONLY, 0);
	(void)open(_PATH_USRBIN, O_RDONLY, 0);
	(void)open(_PATH_USRLIB, O_RDONLY, 0);

	(void)signal(SIGALRM, mysync);

	value.it_interval.tv_sec = 30;
	value.it_interval.tv_usec = 0;
	value.it_value = value.it_interval;
	if (setitimer(ITIMER_REAL, &value, NULL)) {
		perror("update: setitimer");
		exit(1);
	}
	for (;;)
		pause();
	/*NOTREACHED*/
}

/* VARARGS */
void
mysync(i)
	int i;
{
	(void)sync();
}

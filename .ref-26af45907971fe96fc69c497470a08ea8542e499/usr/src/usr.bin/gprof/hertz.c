/*
 * Copyright (c) 1983 Regents of the University of California.
 * All rights reserved.
 *
 * %sccs.include.redist.c%
 */

#ifndef lint
static char sccsid[] = "@(#)hertz.c	5.4 (Berkeley) %G%";
#endif /* not lint */

#include <sys/time.h>

    /*
     *	discover the tick frequency of the machine
     *	if something goes wrong, we return 0, an impossible hertz.
     */
#define	HZ_WRONG	0

hertz()
{
	struct itimerval tim;

	tim.it_interval.tv_sec = 0;
	tim.it_interval.tv_usec = 1;
	tim.it_value.tv_sec = 0;
	tim.it_value.tv_usec = 0;
	setitimer(ITIMER_REAL, &tim, 0);
	setitimer(ITIMER_REAL, 0, &tim);
	if (tim.it_interval.tv_usec < 2)
		return(HZ_WRONG);
	return (1000000 / tim.it_interval.tv_usec);
}

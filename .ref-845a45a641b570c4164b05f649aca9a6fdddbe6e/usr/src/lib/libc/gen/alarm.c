/*
 * Copyright (c) 1983, 1993
 *	The Regents of the University of California.  All rights reserved.
 *
 * %sccs.include.redist.c%
 */

#if defined(LIBC_SCCS) && !defined(lint)
static char sccsid[] = "@(#)alarm.c	8.1 (Berkeley) %G%";
#endif /* LIBC_SCCS and not lint */

/*
 * Backwards compatible alarm.
 */
#include <sys/time.h>
#include <unistd.h>

unsigned int
alarm(secs)
	unsigned int secs;
{
	struct itimerval it, oitv;
	register struct itimerval *itp = &it;

	timerclear(&itp->it_interval);
	itp->it_value.tv_sec = secs;
	itp->it_value.tv_usec = 0;
	if (setitimer(ITIMER_REAL, itp, &oitv) < 0)
		return (-1);
	if (oitv.it_value.tv_usec)
		oitv.it_value.tv_sec++;
	return (oitv.it_value.tv_sec);
}

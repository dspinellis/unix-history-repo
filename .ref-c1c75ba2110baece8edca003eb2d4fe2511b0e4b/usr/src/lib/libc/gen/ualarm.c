/*
 * Copyright (c) 1985 Regents of the University of California.
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

#if defined(LIBC_SCCS) && !defined(lint)
static char sccsid[] = "@(#)ualarm.c	5.3 (Berkeley) %G%";
#endif /* LIBC_SCCS and not lint */

#include <sys/time.h>

#define	USPS	1000000		/* # of microseconds in a second */

/*
 * Generate a SIGALRM signal in ``usecs'' microseconds.
 * If ``reload'' is non-zero, keep generating SIGALRM
 * every ``reload'' microseconds after the first signal.
 */
unsigned
ualarm(usecs, reload)
	register unsigned usecs;
	register unsigned reload;
{
	struct itimerval new, old;

	new.it_interval.tv_usec = reload % USPS;
	new.it_interval.tv_sec = reload / USPS;
	
	new.it_value.tv_usec = usecs % USPS;
	new.it_value.tv_sec = usecs / USPS;

	if (setitimer(ITIMER_REAL, &new, &old) == 0)
		return (old.it_value.tv_sec * USPS + old.it_value.tv_usec);
	/* else */
		return (-1);
}

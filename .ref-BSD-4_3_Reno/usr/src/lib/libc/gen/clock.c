/*
 * Copyright (c) 1989 The Regents of the University of California.
 * All rights reserved.
 *
 * Redistribution and use in source and binary forms are permitted
 * provided that: (1) source distributions retain this entire copyright
 * notice and comment, and (2) distributions including binaries display
 * the following acknowledgement:  ``This product includes software
 * developed by the University of California, Berkeley and its contributors''
 * in the documentation or other materials provided with the distribution
 * and in all advertising materials mentioning features or use of this
 * software. Neither the name of the University nor the names of its
 * contributors may be used to endorse or promote products derived
 * from this software without specific prior written permission.
 * THIS SOFTWARE IS PROVIDED ``AS IS'' AND WITHOUT ANY EXPRESS OR
 * IMPLIED WARRANTIES, INCLUDING, WITHOUT LIMITATION, THE IMPLIED
 * WARRANTIES OF MERCHANTABILITY AND FITNESS FOR A PARTICULAR PURPOSE.
 */

#if defined(LIBC_SCCS) && !defined(lint)
static char sccsid[] = "@(#)clock.c	5.3 (Berkeley) 6/1/90";
#endif /* LIBC_SCCS and not lint */

#include <machine/machlimits.h>
#include <sys/types.h>
#include <sys/time.h>
#include <sys/resource.h>

clock_t
clock()
{
	struct rusage rusage;
	clock_t val;

	if (getrusage(RUSAGE_SELF, &rusage))
		return ((clock_t) -1);
	val = (rusage.ru_utime.tv_sec + rusage.ru_stime.tv_sec) * CLK_TCK;
	/*
	 * Convert usec to clock ticks; could do (usec * CLK_TCK) / 1000000,
	 * but this would overflow if we switch to nanosec.
	 */
	val += (rusage.ru_utime.tv_usec + rusage.ru_stime.tv_usec) /
		(1000000 / CLK_TCK);
	return (val);
}

/*
 * Copyright (c) 1983 The Regents of the University of California.
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
 * WARRANTIES OF MERCHANTABILITY AND FITNESS FOR A PARTICULAR PURPOSE.
 */

#if defined(LIBC_SCCS) && !defined(lint)
static char sccsid[] = "@(#)nice.c	5.3 (Berkeley) %G%";
#endif /* LIBC_SCCS and not lint */

#include <sys/time.h>
#include <sys/resource.h>

/*
 * Backwards compatible nice.
 */
nice(incr)
	int incr;
{
	int prio;
	extern int errno;

	errno = 0;
	prio = getpriority(PRIO_PROCESS, 0);
	if (prio == -1 && errno)
		return (-1);
	return (setpriority(PRIO_PROCESS, 0, prio + incr));
}

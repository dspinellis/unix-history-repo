/*
 * Copyright (c) 1989 The Regents of the University of California.
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
static char sccsid[] = "@(#)clock.c	5.1 (Berkeley) %G%";
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
		return((clock_t)-1);
	val = rusage.ru_utime.tv_sec * CLK_TCK;
	val += rusage.ru_stime.tv_sec * CLK_TCK;
	val += rusage.ru_utime.tv_usec / (1000000/CLK_TCK);
	val += rusage.ru_stime.tv_usec / (1000000/CLK_TCK);
	val += (rusage.ru_utime.tv_usec % (1000000/CLK_TCK) +
	    rusage.ru_utime.tv_usec % (1000000/CLK_TCK)) / (1000000/CLK_TCK);
	return(val);
}

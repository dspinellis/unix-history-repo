/*-
 * Copyright (c) 1979 The Regents of the University of California.
 * All rights reserved.
 *
 * Redistribution and use in source and binary forms are permitted provided
 * that: (1) source distributions retain this entire copyright notice and
 * comment, and (2) distributions including binaries display the following
 * acknowledgement:  ``This product includes software developed by the
 * University of California, Berkeley and its contributors'' in the
 * documentation or other materials provided with the distribution and in
 * all advertising materials mentioning features or use of this software.
 * Neither the name of the University nor the names of its contributors may
 * be used to endorse or promote products derived from this software without
 * specific prior written permission.
 * THIS SOFTWARE IS PROVIDED ``AS IS'' AND WITHOUT ANY EXPRESS OR IMPLIED
 * WARRANTIES, INCLUDING, WITHOUT LIMITATION, THE IMPLIED WARRANTIES OF
 * MERCHANTABILITY AND FITNESS FOR A PARTICULAR PURPOSE.
 */

#ifndef lint
static char sccsid[] = "@(#)CLCK.c	1.4 (Berkeley) 6/29/90";
#endif /* not lint */

#include <sys/time.h>
#include <sys/resource.h>

long
CLCK()
{
	struct rusage ru;

	if (getrusage(RUSAGE_SELF, &ru) < 0)
		return (-1);
	return (ru.ru_utime.tv_sec * 1000 + ru.ru_utime.tv_usec / 1000);
}

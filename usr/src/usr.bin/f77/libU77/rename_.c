/*
 * Copyright (c) 1980 Regents of the University of California.
 * All rights reserved.  The Berkeley software License Agreement
 * specifies the terms and conditions for redistribution.
 *
 *	@(#)rename_.c	5.1	%G%
 */

/*
 * rename a file atomically
 *
 * synopsis:
 *	integer function rename (from, to)
 *	character*(*) from, to
 *
 * where:
 *	return value will be zero normally, an error number otherwise.
 */

#include "../libI77/f_errno.h"
#include <sys/param.h>
#ifndef	MAXPATHLEN
#define MAXPATHLEN	128
#endif

long
rename_ (from, to, frlen, tolen)
char	*from, *to;
long	frlen, tolen;
{
	char	frbuf[MAXPATHLEN];
	char	tobuf[MAXPATHLEN];

	if (frlen <= 0 || tolen <= 0 || *from == ' ' || *to == ' ')
		return ((long)(errno = F_ERARG));
	if (frlen >= sizeof frbuf || tolen >= sizeof tobuf)
		return ((long)(errno = F_ERARG));
	g_char (from, frlen, frbuf);
	g_char (to, tolen, tobuf);
	if (rename (from, to) != 0)
		return ((long)errno);
	return (0L);
}

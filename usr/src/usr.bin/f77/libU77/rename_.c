/*-
 * Copyright (c) 1980 The Regents of the University of California.
 * All rights reserved.
 *
 * This module is believed to contain source code proprietary to AT&T.
 * Use and redistribution is subject to the Berkeley Software License
 * Agreement and your Software Agreement with AT&T (Western Electric).
 */

#ifndef lint
static char sccsid[] = "@(#)rename_.c	5.2 (Berkeley) 4/12/91";
#endif /* not lint */

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

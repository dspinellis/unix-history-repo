/*
 * Copyright (c) 1985 Regents of the University of California.
 * All rights reserved.  The Berkeley software License Agreement
 * specifies the terms and conditions for redistribution.
 */

/*
 * Sys5 compat routine
 */

#if defined(LIBC_SCCS) && !defined(lint)
static char sccsid[] = "@(#)memset.c	5.2 (Berkeley) 86/03/09";
#endif

char *
memset(s, c, n)
	register char *s;
	register c, n;
{
	register char *p = s;

	while (--n >= 0)
		*s++ = c;

	return (p);
}

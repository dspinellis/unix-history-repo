/*
 * Copyright (c) 1985 Regents of the University of California.
 * All rights reserved.  The Berkeley software License Agreement
 * specifies the terms and conditions for redistribution.
 */

/*
 * Sys5 compat routine
 */

#ifndef lint
static char sccsid[] = "@(#)memset.c	5.1 (Berkeley) 85/08/05";
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

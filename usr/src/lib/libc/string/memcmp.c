/*
 * Copyright (c) 1985 Regents of the University of California.
 * All rights reserved.  The Berkeley software License Agreement
 * specifies the terms and conditions for redistribution.
 */

/*
 * Sys5 compat routine
 */

#ifndef lint
static char sccsid[] = "@(#)memcmp.c	5.1 (Berkeley) 85/08/05";
#endif

memcmp(s1, s2, n)
	register char *s1, *s2;
	register n;
{
	while (--n >= 0)
		if (*s1++ != *s2++)
			return (*--s1 - *--s2);
	return (0);
}

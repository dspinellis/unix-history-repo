/*
 * Copyright (c) 1985 Regents of the University of California.
 * All rights reserved.  The Berkeley software License Agreement
 * specifies the terms and conditions for redistribution.
 */

/*
 * Sys5 compat routine
 */

#ifndef lint
static char sccsid[] = "@(#)memccpy.c	5.1 (Berkeley) 85/08/05";
#endif


char *
memccpy(t, f, c, n)
	register char *t, *f;
	register c, n;
{
	while (--n >= 0)
		if ((*t++ = *f++) == c)
			return (t);
	return (0);
}

/*
 * Copyright (c) 1985 Regents of the University of California.
 * All rights reserved.  The Berkeley software License Agreement
 * specifies the terms and conditions for redistribution.
 */

/*
 * Sys5 compat routine
 */

#ifndef lint
static char sccsid[] = "@(#)memcpy.c	5.1 (Berkeley) 85/08/05";
#endif

char *
memcpy(t, f, n)
	register char *t, *f;
	register n;
{
	register char *p = t;

	while (--n >= 0)
		*t++ = *f++;

	return (p);
}

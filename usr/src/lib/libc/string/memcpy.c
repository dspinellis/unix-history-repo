/*
 * Copyright (c) 1985 Regents of the University of California.
 * All rights reserved.  The Berkeley software License Agreement
 * specifies the terms and conditions for redistribution.
 */

/*
 * Sys5 compat routine
 */

#if defined(LIBC_SCCS) && !defined(lint)
static char sccsid[] = "@(#)memcpy.c	5.2 (Berkeley) 86/03/09";
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

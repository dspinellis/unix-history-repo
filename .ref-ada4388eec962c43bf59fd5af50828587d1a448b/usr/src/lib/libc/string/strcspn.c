/*-
 * Copyright (c) 1990, 1993
 *	The Regents of the University of California.  All rights reserved.
 *
 * This code is derived from software contributed to Berkeley by
 * Chris Torek.
 *
 * %sccs.include.redist.c%
 */

#if defined(LIBC_SCCS) && !defined(lint)
static char sccsid[] = "@(#)strcspn.c	8.1 (Berkeley) %G%";
#endif /* LIBC_SCCS and not lint */

#include <sys/cdefs.h>
#include <string.h>

/*
 * Span the complement of string s2.
 */
size_t
strcspn(s1, s2)
	const char *s1;
	register const char *s2;
{
	register const char *p, *spanp;
	register char c, sc;

	/*
	 * Stop as soon as we find any character from s2.  Note that there
	 * must be a NUL in s2; it suffices to stop when we find that, too.
	 */
	for (p = s1;;) {
		c = *p++;
		spanp = s2;
		do {
			if ((sc = *spanp++) == c)
				return (p - 1 - s1);
		} while (sc != 0);
	}
	/* NOTREACHED */
}

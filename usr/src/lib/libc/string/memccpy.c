/*-
 * Copyright (c) 1990, 1993
 *	The Regents of the University of California.  All rights reserved.
 *
 * %sccs.include.redist.c%
 */

#if defined(LIBC_SCCS) && !defined(lint)
static char sccsid[] = "@(#)memccpy.c	8.1 (Berkeley) %G%";
#endif /* LIBC_SCCS and not lint */

#include <sys/cdefs.h>
#include <string.h>

void *
memccpy(t, f, c, n)
	void *t;
	const void *f;
	int c;
	register size_t n;
{

	if (n) {
		register unsigned char *tp = t;
		register const unsigned char *fp = f;
		do {
			if ((*tp++ = *fp++) == c)
				return (t);
		} while (--n != 0);
	}
	return (0);
}

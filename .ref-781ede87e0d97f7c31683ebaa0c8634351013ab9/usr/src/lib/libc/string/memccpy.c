/*-
 * Copyright (c) 1990 The Regents of the University of California.
 * All rights reserved.
 *
 * %sccs.include.redist.c%
 */

#if defined(LIBC_SCCS) && !defined(lint)
static char sccsid[] = "@(#)memccpy.c	5.6 (Berkeley) %G%";
#endif /* LIBC_SCCS and not lint */

#include <string.h>
#include <sys/stdc.h>

void *
memccpy(t, f, c, n)
	void *t;
	const void *f;
	int c;
	register size_t n;
{

	if (n) {
		register unsigned char *t;
		register const unsigned char *f;
		register unsigned char ch = c;

		do {
			if ((*t++ = *f++) == c)
				return (t);
		} while (--n != 0);
	}
	return (0);
}

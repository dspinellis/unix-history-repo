/*-
 * Copyright (c) 1990 The Regents of the University of California.
 * All rights reserved.
 *
 * This code is derived from software contributed to Berkeley by
 * Chris Torek.
 *
 * %sccs.include.redist.c%
 */

#if defined(LIBC_SCCS) && !defined(lint)
static char sccsid[] = "@(#)memset.c	5.5 (Berkeley) %G%";
#endif /* LIBC_SCCS and not lint */

#include <string.h>
#include <sys/stdc.h>

void *
memset(dst, c, n)
	void *dst;
	register int c;
	register size_t n;
{

	if (n != 0) {
		register char *d = dst;

		do
			*d++ = c;
		while (--n != 0);
	}
	return (dst);
}

/*-
 * Copyright (c) 1990 The Regents of the University of California.
 * All rights reserved.
 *
 * %sccs.include.redist.c%
 */

#if defined(LIBC_SCCS) && !defined(lint)
static char sccsid[] = "@(#)memccpy.c	5.5 (Berkeley) %G%";
#endif /* LIBC_SCCS and not lint */

#include <string.h>
#include <sys/stdc.h>

void *
memccpy(t, f, c, n)
	register const unsigned char *t, *f;
	register unsigned char c;
	register size_t n;
{
	while (--n >= 0)
		if ((*t++ = *f++) == c)
			return (t);
	return (0);
}

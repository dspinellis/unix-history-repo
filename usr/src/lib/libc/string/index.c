/*-
 * Copyright (c) 1990 The Regents of the University of California.
 * All rights reserved.
 *
 * %sccs.include.redist.c%
 */

#if defined(LIBC_SCCS) && !defined(lint)
static char sccsid[] = "@(#)index.c	5.5 (Berkeley) %G%";
#endif /* LIBC_SCCS and not lint */

#include <string.h>
#include <stddef.h>

char *
index(p, ch)
	register char *p, ch;
{
	for (;; ++p) {
		if (*p == ch)
			return(p);
		if (!*p)
			return((char *)NULL);
	}
	/* NOTREACHED */
}

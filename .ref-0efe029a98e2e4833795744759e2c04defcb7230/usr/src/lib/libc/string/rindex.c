/*
 * Copyright (c) 1988 Regents of the University of California.
 * All rights reserved.
 *
 * %sccs.include.redist.c%
 */

#if defined(LIBC_SCCS) && !defined(lint)
static char sccsid[] = "@(#)rindex.c	5.8 (Berkeley) %G%";
#endif /* LIBC_SCCS and not lint */

#include <stddef.h>
#include <string.h>

char *
#ifdef STRRCHR
strrchr(p, ch)
#else
rindex(p, ch)
#endif
	register char *p, ch;
{
	register char *save;

	for (save = NULL;; ++p) {
		if (*p == ch)
			save = p;
		if (!*p)
			return(save);
	}
	/* NOTREACHED */
}

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
static char sccsid[] = "@(#)fwalk.c	8.1 (Berkeley) %G%";
#endif /* LIBC_SCCS and not lint */

#include <errno.h>
#include <stdio.h>
#include "local.h"
#include "glue.h"

_fwalk(function)
	register int (*function)();
{
	register FILE *fp;
	register int n, ret;
	register struct glue *g;

	ret = 0;
	for (g = &__sglue; g != NULL; g = g->next)
		for (fp = g->iobs, n = g->niobs; --n >= 0; fp++)
			if (fp->_flags != 0)
				ret |= (*function)(fp);
	return (ret);
}

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
static char sccsid[] = "@(#)fpurge.c	8.1 (Berkeley) %G%";
#endif /* LIBC_SCCS and not lint */

#include <errno.h>
#include <stdio.h>
#include <stdlib.h>
#include "local.h"

/*
 * fpurge: like fflush, but without writing anything: leave the
 * given FILE's buffer empty.
 */
int
fpurge(fp)
	register FILE *fp;
{
	if (!fp->_flags) {
		errno = EBADF;
		return(EOF);
	}

	if (HASUB(fp))
		FREEUB(fp);
	fp->_p = fp->_bf._base;
	fp->_r = 0;
	fp->_w = fp->_flags & (__SLBF|__SNBF) ? 0 : fp->_bf._size;
	return (0);
}

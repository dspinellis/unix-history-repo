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
static char sccsid[] = "@(#)fflush.c	5.2 (Berkeley) %G%";
#endif /* LIBC_SCCS and not lint */

#include <sys/errno.h>
#include <stdio.h>
#include "local.h"

/* Flush a single file, or (if fp is NULL) all files.  */
fflush(fp)
	register FILE *fp;
{

	if (fp == NULL)
		return (_fwalk(__sflush));
	if ((fp->_flags & (__SWR | __SRW)) == 0) {
		errno = EBADF;
		return (EOF);
	}
	return (__sflush(fp));
}

__sflush(fp)
	register FILE *fp;
{
	register unsigned char *p;
	register int n, t;

	t = fp->_flags;
	if ((t & __SWR) == 0)
		return (0);

	if ((p = fp->_bf._base) == NULL)
		return (0);

	n = fp->_p - p;		/* write this much */

	/*
	 * Set these immediately to avoid problems with longjmp and to allow
	 * exchange buffering (via setvbuf) in user write function.
	 */
	fp->_p = p;
	fp->_w = t & (__SLBF|__SNBF) ? 0 : fp->_bf._size;

	for (; n > 0; n -= t, p += t) {
		t = (*fp->_write)(fp->_cookie, (char *)p, n);
		if (t <= 0) {
			fp->_flags |= __SERR;
			return (EOF);
		}
	}
	return (0);
}

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
static char sccsid[] = "@(#)funopen.c	8.1 (Berkeley) %G%";
#endif /* LIBC_SCCS and not lint */

#include <stdio.h>
#include <errno.h>
#include "local.h"

FILE *
funopen(cookie, readfn, writefn, seekfn, closefn)
	const void *cookie;
	int (*readfn)(), (*writefn)();
#if __STDC__
	fpos_t (*seekfn)(void *cookie, fpos_t off, int whence);
#else
	fpos_t (*seekfn)();
#endif
	int (*closefn)();
{
	register FILE *fp;
	int flags;

	if (readfn == NULL) {
		if (writefn == NULL) {		/* illegal */
			errno = EINVAL;
			return (NULL);
		} else
			flags = __SWR;		/* write only */
	} else {
		if (writefn == NULL)
			flags = __SRD;		/* read only */
		else
			flags = __SRW;		/* read-write */
	}
	if ((fp = __sfp()) == NULL)
		return (NULL);
	fp->_flags = flags;
	fp->_file = -1;
	fp->_cookie = (void *)cookie;
	fp->_read = readfn;
	fp->_write = writefn;
	fp->_seek = seekfn;
	fp->_close = closefn;
	return (fp);
}

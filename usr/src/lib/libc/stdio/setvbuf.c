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
static char sccsid[] = "@(#)setvbuf.c	5.3 (Berkeley) %G%";
#endif /* LIBC_SCCS and not lint */

#include <stdio.h>
#include <stdlib.h>
#include "local.h"

/*
 * Set one of the three kinds of buffering, optionally including
 * a buffer.
 */
setvbuf(fp, buf, mode, size)
	register FILE *fp;
	char *buf;
	register int mode;
	register size_t size;
{
	if (buf == NULL)
		size = 0;

	/*
	 * Verify arguments.  The `int' limit on `size' is due to this
	 * particular implementation.
	 */
	if ((mode != _IOFBF && mode != _IOLBF && mode != _IONBF) ||
	    (int)size < 0)
		return (EOF);

	/*
	 * Write current buffer, if any; drop read count, if any.
	 * Make sure putc() will not think fp is line buffered.
	 * Free old buffer if it was from malloc().  Clear line and
	 * non buffer flags, and clear malloc flag.
	 */
	(void) __sflush(fp);
	fp->_r = 0;
	fp->_lbfsize = 0;
	if (fp->_flags & __SMBF)
		free((void *)fp->_bf._base);
	fp->_flags &= ~(__SLBF|__SNBF|__SMBF);

	/*
	 * Now put back whichever flag is needed, and fix _lbfsize
	 * if line buffered.  Ensure output flush on exit if the
	 * stream will be buffered at all.
	 */
	switch (mode) {

	case _IONBF:
		fp->_flags |= __SNBF;
		fp->_bf._base = fp->_p = fp->_nbuf;
		fp->_bf._size = 1;
		break;

	case _IOLBF:
		fp->_flags |= __SLBF;
		fp->_lbfsize = -size;
		/* FALLTHROUGH */

	case _IOFBF:
		/* no flag */
		__cleanup = _cleanup;
		fp->_bf._base = fp->_p = (unsigned char *)buf;
		fp->_bf._size = size;
		break;
	}

	/*
	 * Patch up write count if necessary.
	 */
	if (fp->_flags & __SWR)
		fp->_w = fp->_flags & (__SLBF|__SNBF) ? 0 : size;

	return (0);
}

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
static char sccsid[] = "@(#)setvbuf.c	8.2 (Berkeley) %G%";
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
	register int ret, flags;
	size_t iosize;
	int ttyflag;

	/*
	 * Verify arguments.  The `int' limit on `size' is due to this
	 * particular implementation.  Note, buf and size are ignored
	 * when setting _IONBF.
	 */
	if (mode != _IONBF)
		if ((mode != _IOFBF && mode != _IOLBF) || (int)size < 0)
			return (EOF);

	/*
	 * Write current buffer, if any.  Discard unread input (including
	 * ungetc data), cancel line buffering, and free old buffer if
	 * malloc()ed.  We also clear any eof condition, as if this were
	 * a seek.
	 */
	ret = 0;
	(void)__sflush(fp);
	if (HASUB(fp))
		FREEUB(fp);
	fp->_r = fp->_lbfsize = 0;
	flags = fp->_flags;
	if (flags & __SMBF)
		free((void *)fp->_bf._base);
	flags &= ~(__SLBF | __SNBF | __SMBF | __SOPT | __SNPT | __SEOF);

	/* If setting unbuffered mode, skip all the hard work. */
	if (mode == _IONBF)
		goto nbf;

	/*
	 * Find optimal I/O size for seek optimization.  This also returns
	 * a `tty flag' to suggest that we check isatty(fd), but we do not
	 * care since our caller told us how to buffer.
	 */
	flags |= __swhatbuf(fp, &iosize, &ttyflag);
	if (size == 0) {
		buf = NULL;	/* force local allocation */
		size = iosize;
	}

	/* Allocate buffer if needed. */
	if (buf == NULL) {
		if ((buf = malloc(size)) == NULL) {
			/*
			 * Unable to honor user's request.  We will return
			 * failure, but try again with file system size.
			 */
			ret = EOF;
			if (size != iosize) {
				size = iosize;
				buf = malloc(size);
			}
		}
		if (buf == NULL) {
			/* No luck; switch to unbuffered I/O. */
nbf:
			fp->_flags = flags | __SNBF;
			fp->_w = 0;
			fp->_bf._base = fp->_p = fp->_nbuf;
			fp->_bf._size = 1;
			return (ret);
		}
		flags |= __SMBF;
	}

	/*
	 * Kill any seek optimization if the buffer is not the
	 * right size.
	 *
	 * SHOULD WE ALLOW MULTIPLES HERE (i.e., ok iff (size % iosize) == 0)?
	 */
	if (size != iosize)
		flags |= __SNPT;

	/*
	 * Fix up the FILE fields, and set __cleanup for output flush on
	 * exit (since we are buffered in some way).
	 */
	if (mode == _IOLBF)
		flags |= __SLBF;
	fp->_flags = flags;
	fp->_bf._base = fp->_p = (unsigned char *)buf;
	fp->_bf._size = size;
	/* fp->_lbfsize is still 0 */
	if (flags & __SWR) {
		/*
		 * Begin or continue writing: see __swsetup().  Note
		 * that __SNBF is impossible (it was handled earlier).
		 */
		if (flags & __SLBF) {
			fp->_w = 0;
			fp->_lbfsize = -fp->_bf._size;
		} else
			fp->_w = size;
	} else {
		/* begin/continue reading, or stay in intermediate state */
		fp->_w = 0;
	}
	__cleanup = _cleanup;

	return (ret);
}

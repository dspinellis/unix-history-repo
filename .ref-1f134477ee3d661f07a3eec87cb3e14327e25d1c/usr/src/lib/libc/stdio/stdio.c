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
static char sccsid[] = "@(#)stdio.c	8.1 (Berkeley) %G%";
#endif /* LIBC_SCCS and not lint */

#include <fcntl.h>
#include <unistd.h>
#include <stdio.h>
#include "local.h"

/*
 * Small standard I/O/seek/close functions.
 * These maintain the `known seek offset' for seek optimisation.
 */
__sread(cookie, buf, n)
	void *cookie;
	char *buf;
	int n;
{
	register FILE *fp = cookie;
	register int ret;
	
	ret = read(fp->_file, buf, n);
	/* if the read succeeded, update the current offset */
	if (ret >= 0)
		fp->_offset += ret;
	else
		fp->_flags &= ~__SOFF;	/* paranoia */
	return (ret);
}

__swrite(cookie, buf, n)
	void *cookie;
	char const *buf;
	int n;
{
	register FILE *fp = cookie;

	if (fp->_flags & __SAPP)
		(void) lseek(fp->_file, (off_t)0, SEEK_END);
	fp->_flags &= ~__SOFF;	/* in case FAPPEND mode is set */
	return (write(fp->_file, buf, n));
}

fpos_t
__sseek(cookie, offset, whence)
	void *cookie;
	fpos_t offset;
	int whence;
{
	register FILE *fp = cookie;
	register off_t ret;
	
	ret = lseek(fp->_file, (off_t)offset, whence);
	if (ret == -1L)
		fp->_flags &= ~__SOFF;
	else {
		fp->_flags |= __SOFF;
		fp->_offset = ret;
	}
	return (ret);
}

__sclose(cookie)
	void *cookie;
{

	return (close(((FILE *)cookie)->_file));
}

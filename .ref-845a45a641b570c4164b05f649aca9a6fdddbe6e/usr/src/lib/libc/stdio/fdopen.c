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
static char sccsid[] = "@(#)fdopen.c	8.1 (Berkeley) %G%";
#endif /* LIBC_SCCS and not lint */

#include <sys/types.h>
#include <fcntl.h>
#include <unistd.h>
#include <stdio.h>
#include <errno.h>
#include "local.h"

FILE *
fdopen(fd, mode)
	int fd;
	const char *mode;
{
	register FILE *fp;
	static int nofile;
	int flags, oflags, fdflags, tmp;

	if (nofile == 0)
		nofile = getdtablesize();

	if ((flags = __sflags(mode, &oflags)) == 0)
		return (NULL);

	/* Make sure the mode the user wants is a subset of the actual mode. */
	if ((fdflags = fcntl(fd, F_GETFL, 0)) < 0)
		return (NULL);
	tmp = fdflags & O_ACCMODE;
	if (tmp != O_RDWR && (tmp != (oflags & O_ACCMODE))) {
		errno = EINVAL;
		return (NULL);
	}

	if ((fp = __sfp()) == NULL)
		return (NULL);
	fp->_flags = flags;
	/*
	 * If opened for appending, but underlying descriptor does not have
	 * O_APPEND bit set, assert __SAPP so that __swrite() will lseek to
	 * end before each write.
	 */
	if ((oflags & O_APPEND) && !(fdflags & O_APPEND))
		fp->_flags |= __SAPP;
	fp->_file = fd;
	fp->_cookie = fp;
	fp->_read = __sread;
	fp->_write = __swrite;
	fp->_seek = __sseek;
	fp->_close = __sclose;
	return (fp);
}

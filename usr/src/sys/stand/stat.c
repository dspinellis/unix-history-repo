/*-
 * Copyright (c) 1993 The Regents of the University of California.
 * All rights reserved.
 *
 * %sccs.include.redist.c%
 *
 *	@(#)stat.c	7.1 (Berkeley) %G%
 */

#include <stand/stand.h>

fstat(fd, sb)
	int fd;
	struct stat *sb;
{
	register struct open_file *f = &files[fd];

	if ((unsigned)fd >= SOPEN_MAX || f->f_flags == 0) {
		errno = EBADF;
		return (-1);
	}

	/* operation not defined on raw devices */
	if (f->f_flags & F_RAW) {
		errno = EOPNOTSUPP;
		return (-1);
	}

	errno = (f->f_ops->stat)(f, sb);
	return (0);
}

stat(str, sb)
	const char *str;
	struct stat *sb;
{
	int fd, rv;

	fd = open(str, 0);
	if (fd < 0)
		return (-1);
	rv = fstat(fd, sb);
	(void)close(fd);
	return (rv);
}

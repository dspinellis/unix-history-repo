/*-
 * Copyright (c) 1991 The Regents of the University of California.
 * All rights reserved.
 *
 * %sccs.include.redist.c%
 *
 *	@(#)stat.c	7.2 (Berkeley) %G%
 */

#include <sys/param.h>
#include <sys/stat.h>
#include <stand/saio.h>

#ifndef SMALL
fstat(fd, sb)
	int fd;
	struct stat *sb;
{
	register struct iob *io;

	fd -= 3;
	if (fd < 0 || fd >= SOPEN_MAX ||
	    ((io = &iob[fd])->i_flgs & F_ALLOC) == 0) {
		errno = EBADF;
		return (-1);
	}
	/* only important stuff */
	sb->st_mode = io->i_ino.di_mode;
	sb->st_uid = io->i_ino.di_uid;
	sb->st_gid = io->i_ino.di_gid;
	sb->st_size = io->i_ino.di_size;
	return (0);
}

stat(str, sb)
	const char *str;
	struct stat *sb;
{
	int fd, rv;

	fd = open(str, 0);
	if (fd < 0)
		return(-1);
	rv = fstat(fd, sb);
	close(fd);
	return(rv);
}
#endif SMALL

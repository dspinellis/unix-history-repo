/*
 * Copyright (c) 1983 Regents of the University of California.
 * All rights reserved.
 *
 * %sccs.include.redist.c%
 */

#if defined(LIBC_SCCS) && !defined(lint)
static char sccsid[] = "@(#)closedir.c	5.8 (Berkeley) %G%";
#endif /* LIBC_SCCS and not lint */

#include <sys/types.h>
#include <dirent.h>

/*
 * close a directory.
 */
closedir(dirp)
	register DIR *dirp;
{
	int fd;

	fd = dirp->dd_fd;
	dirp->dd_fd = -1;
	dirp->dd_loc = 0;
	(void)free((void *)dirp->dd_buf);
	(void)free((void *)dirp);
	return(close(fd));
}

/*
 * Copyright (c) 1983 Regents of the University of California.
 * All rights reserved.
 *
 * %sccs.include.redist.c%
 */

#if defined(LIBC_SCCS) && !defined(lint)
static char sccsid[] = "@(#)opendir.c	8.1 (Berkeley) %G%";
#endif /* LIBC_SCCS and not lint */

#include <sys/param.h>
#include <dirent.h>
#include <fcntl.h>
#include <stdlib.h>
#include <unistd.h>

long _rewinddir;

/*
 * open a directory.
 */
DIR *
opendir(name)
	const char *name;
{
	register DIR *dirp;
	register int fd;

	if ((fd = open(name, 0)) == -1)
		return NULL;
	if (fcntl(fd, F_SETFD, FD_CLOEXEC) == -1 ||
	    (dirp = (DIR *)malloc(sizeof(DIR))) == NULL) {
		close (fd);
		return NULL;
	}
	/*
	 * If CLBYTES is an exact multiple of DIRBLKSIZ, use a CLBYTES
	 * buffer that it cluster boundary aligned.
	 * Hopefully this can be a big win someday by allowing page trades
	 * to user space to be done by getdirentries()
	 */
	if ((CLBYTES % DIRBLKSIZ) == 0) {
		dirp->dd_buf = malloc(CLBYTES);
		dirp->dd_len = CLBYTES;
	} else {
		dirp->dd_buf = malloc(DIRBLKSIZ);
		dirp->dd_len = DIRBLKSIZ;
	}
	if (dirp->dd_buf == NULL) {
		close (fd);
		return NULL;
	}
	dirp->dd_fd = fd;
	dirp->dd_loc = 0;
	dirp->dd_seek = 0;
	/*
	 * Set up seek point for rewinddir.
	 */
	_rewinddir = telldir(dirp);
	return dirp;
}

/*
 * Copyright (c) 1988 The Regents of the University of California.
 * All rights reserved.
 *
 * %sccs.include.redist.c%
 */

#if defined(LIBC_SCCS) && !defined(lint)
static char sccsid[] = "@(#)ttyname.c	5.6 (Berkeley) %G%";
#endif /* LIBC_SCCS and not lint */

#include <sys/types.h>
#include <sys/dir.h>
#include <sys/stat.h>
#include <sgtty.h>
#include <paths.h>

char *
ttyname(fd)
	int fd;
{
	register struct direct *dirp;
	register DIR *dp;
	struct stat sb1, sb2;
	struct sgttyb ttyb;
	static char buf[sizeof(_PATH_DEV) + MAXNAMLEN] = _PATH_DEV;
	char *rval, *strcpy();

	if (ioctl(fd, TIOCGETP, &ttyb) < 0)
		return(NULL);
	if (fstat(fd, &sb1) < 0 || (sb1.st_mode&S_IFMT) != S_IFCHR)
		return(NULL);
	if ((dp = opendir(_PATH_DEV)) == NULL)
		return(NULL);
	for (rval = NULL; dirp = readdir(dp);) {
		if (dirp->d_ino != sb1.st_ino)
			continue;
		(void)strcpy(buf + sizeof(_PATH_DEV) - 1, dirp->d_name);
		if (stat(buf, &sb2) < 0 || sb1.st_dev != sb2.st_dev ||
		    sb1.st_ino != sb2.st_ino)
			continue;
		closedir(dp);
		rval = buf;
		break;
	}
	closedir(dp);
	return(rval);
}

/*
 * Copyright (c) 1988 The Regents of the University of California.
 * All rights reserved.
 *
 * %sccs.include.redist.c%
 */

#if defined(LIBC_SCCS) && !defined(lint)
static char sccsid[] = "@(#)ttyname.c	5.7 (Berkeley) %G%";
#endif /* LIBC_SCCS and not lint */

#include <sys/types.h>
#include <sys/stat.h>
#include <fcntl.h>
#include <dirent.h>
#include <sgtty.h>
#include <ndbm.h>
#include <paths.h>

static char buf[sizeof(_PATH_DEV) + MAXNAMLEN] = _PATH_DEV;

char *
ttyname(fd)
	int fd;
{
	struct stat sb;
	struct sgttyb ttyb;
	DBM *db;
	datum dp, key;
	static char *__oldttyname();

	/* Must be a terminal. */
	if (ioctl(fd, TIOCGETP, &ttyb) < 0)
		return(NULL);
	/* Must be a character device. */
	if (fstat(fd, &sb) || !S_ISCHR(sb.st_mode))
		return(NULL);

	if (!(db = dbm_open(_PATH_DEVDB, O_RDONLY, 0)))
		return(__oldttyname(fd, &sb));
	key.dptr = (char *)&sb.st_rdev;
	key.dsize = sizeof(sb.st_rdev);
	dp = dbm_fetch(db, key);
	if (!dp.dptr)
		return(__oldttyname(fd, &sb));
	bcopy(dp.dptr, buf + sizeof(_PATH_DEV) - 1, dp.dsize);
	return(buf);
}

static char *
__oldttyname(fd, sb)
	int fd;
	struct stat *sb;
{
	register struct dirent *dirp;
	register DIR *dp;
	struct stat dsb;
	char *rval, *strcpy();

	if ((dp = opendir(_PATH_DEV)) == NULL)
		return(NULL);

	for (rval = NULL; dirp = readdir(dp);) {
		if (dirp->d_fileno != sb->st_ino)
			continue;
		bcopy(dirp->d_name, buf + sizeof(_PATH_DEV) - 1,
		    dirp->d_namlen + 1);
		if (stat(buf, &dsb) || sb->st_dev != dsb.st_dev ||
		    sb->st_ino != dsb.st_ino)
			continue;
		(void)closedir(dp);
		rval = buf;
		break;
	}
	(void)closedir(dp);
	return(rval);
}

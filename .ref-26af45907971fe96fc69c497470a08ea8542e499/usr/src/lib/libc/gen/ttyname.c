/*
 * Copyright (c) 1988, 1993
 *	The Regents of the University of California.  All rights reserved.
 *
 * %sccs.include.redist.c%
 */

#if defined(LIBC_SCCS) && !defined(lint)
static char sccsid[] = "@(#)ttyname.c	8.1 (Berkeley) %G%";
#endif /* LIBC_SCCS and not lint */

#include <sys/types.h>
#include <sys/stat.h>
#include <fcntl.h>
#include <dirent.h>
#include <sgtty.h>
#include <db.h>
#include <string.h>
#include <paths.h>

static char buf[sizeof(_PATH_DEV) + MAXNAMLEN] = _PATH_DEV;
static char *oldttyname __P((int, struct stat *));

char *
ttyname(fd)
	int fd;
{
	struct stat sb;
	struct sgttyb ttyb;
	DB *db;
	DBT data, key;
	struct {
		mode_t type;
		dev_t dev;
	} bkey;

	/* Must be a terminal. */
	if (ioctl(fd, TIOCGETP, &ttyb) < 0)
		return (NULL);
	/* Must be a character device. */
	if (fstat(fd, &sb) || !S_ISCHR(sb.st_mode))
		return (NULL);

	if (db = dbopen(_PATH_DEVDB, O_RDONLY, 0, DB_HASH, NULL)) {
		bkey.type = S_IFCHR;
		bkey.dev = sb.st_rdev;
		key.data = &bkey;
		key.size = sizeof(bkey);
		if (!(db->get)(db, &key, &data, 0)) {
			bcopy(data.data,
			    buf + sizeof(_PATH_DEV) - 1, data.size);
			(void)(db->close)(db);
			return (buf);
		}
		(void)(db->close)(db);
	}
	return (oldttyname(fd, &sb));
}

static char *
oldttyname(fd, sb)
	int fd;
	struct stat *sb;
{
	register struct dirent *dirp;
	register DIR *dp;
	struct stat dsb;

	if ((dp = opendir(_PATH_DEV)) == NULL)
		return (NULL);

	while (dirp = readdir(dp)) {
		if (dirp->d_fileno != sb->st_ino)
			continue;
		bcopy(dirp->d_name, buf + sizeof(_PATH_DEV) - 1,
		    dirp->d_namlen + 1);
		if (stat(buf, &dsb) || sb->st_dev != dsb.st_dev ||
		    sb->st_ino != dsb.st_ino)
			continue;
		(void)closedir(dp);
		return (buf);
	}
	(void)closedir(dp);
	return (NULL);
}

/*
 * Copyright (c) 1989 The Regents of the University of California.
 * All rights reserved.
 *
 * %sccs.include.redist.c%
 */

#if defined(LIBC_SCCS) && !defined(lint)
static char sccsid[] = "@(#)getcwd.c	5.8 (Berkeley) %G%";
#endif /* LIBC_SCCS and not lint */

#include <sys/param.h>
#include <sys/stat.h>
#include <dirent.h>
#include <string.h>

#define	ISDOT(dp) \
	(dp->d_name[0] == '.' && (dp->d_name[1] == '\0' || \
	    dp->d_name[1] == '.' && dp->d_name[2] == '\0'))

char *
getwd(store)
	char *store;
{
	extern int errno;
	register struct dirent *dp;
	register DIR *dir;
	register ino_t ino;
	register char *pp, *pu;
	register int first;
	struct stat s;
	dev_t root_dev, dev;
	ino_t root_ino;
	int save_errno, found;
	char path[MAXPATHLEN], up[MAXPATHLEN], *file;

	/* save root values */
	if (stat("/", &s)) {
		file = "/";
		goto err;
	}
	root_dev = s.st_dev;
	root_ino = s.st_ino;

	/* init path pointer; built from the end of the buffer */
	pp = path + sizeof(path) - 1;
	*pp = '\0';

	/* special case first stat, it's ".", not ".." */
	up[0] = '.';
	up[1] = '\0';

	for (pu = up, first = 1;; first = 0) {
		/* stat current level */
		if (lstat(up, &s)) {
			file = up;
			goto err;
		}

		/* save current node values */
		ino = s.st_ino;
		dev = s.st_dev;

		/* check for root */
		if (root_dev == dev && root_ino == ino) {
			*store = '/';
			(void) strcpy(store + 1, pp);
			return (store);
		}

		*pu++ = '.';
		*pu++ = '.';
		*pu = '\0';

		/* open and stat parent */
		if (!(dir = opendir(up)) || fstat(dirfd(dir), &s)) {
			file = up;
			goto err;
		}
		found = save_errno = 0;

		*pu++ = '/';

		/*
		 * if it's a mount point you have to stat each element because
		 * the inode number in the directory is for the entry in the
		 * parent directory, not the inode number of the mounted file.
		 */
		if (s.st_dev == dev) {
			while (dp = readdir(dir))
				if (dp->d_fileno == ino)
					goto hit;
		} else {
			while (dp = readdir(dir)) {
				if (ISDOT(dp))
					continue;
				bcopy(dp->d_name, pu, dp->d_namlen + 1);
				if (lstat(up, &s)) {
					file = dp->d_name;
					save_errno = errno;
					errno = 0;
					continue;
				}
				if (s.st_dev == dev && s.st_ino == ino) {
hit:					if (!first)
						*--pp = '/';
					pp -= dp->d_namlen;
					bcopy(dp->d_name, pp, dp->d_namlen);
					found = 1;
					break;
				}
			}
			if (errno) {
				file = up;
				save_errno = errno;
			}
		}
		(void) closedir(dir);

		*pu = '\0';

		if (!found) {
			/*
			 * We didn't find the current level in its parent
			 * directory; figure out what to complain about.
			 */
			if (save_errno) {
				errno = save_errno;
				goto err;
			}
			(void) sprintf(store, "%s not found in %s?\n",
				first ? "." : pp, up);
			return ((char *)NULL);
		}
	}
err:
	(void) sprintf(store, "getwd: %s: %s", file, strerror(errno));
	return ((char *)NULL);
}

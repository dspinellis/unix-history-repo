/*
 * Copyright (c) 1989 The Regents of the University of California.
 * All rights reserved.
 *
 * Redistribution and use in source and binary forms are permitted
 * provided that the above copyright notice and this paragraph are
 * duplicated in all such forms and that any documentation,
 * advertising materials, and other materials related to such
 * distribution and use acknowledge that the software was developed
 * by the University of California, Berkeley.  The name of the
 * University may not be used to endorse or promote products derived
 * from this software without specific prior written permission.
 * THIS SOFTWARE IS PROVIDED ``AS IS'' AND WITHOUT ANY EXPRESS OR
 * IMPLIED WARRANTIES, INCLUDING, WITHOUT LIMITATION, THE IMPLIED
 * WARRANTIES OF MERCHANTABILITY AND FITNESS FOR A PARTICULAR PURPOSE.
 */

#if defined(LIBC_SCCS) && !defined(lint)
static char sccsid[] = "@(#)getcwd.c	5.6 (Berkeley) %G%";
#endif /* LIBC_SCCS and not lint */

#include <sys/param.h>
#include <sys/stat.h>
#include <dirent.h>

char *
getwd(store)
	char *store;
{
	extern int errno;
	register DIR *dir;
	register struct dirent *dp;
	register int first;
	register char *pp, *pu;
	struct stat s, tmp;
	dev_t root_dev;
	ino_t root_ino;
	int save_errno, found;
	char path[MAXPATHLEN], up[MAXPATHLEN], *file, *strerror();

	if (stat("/", &s)) {
		file = "/";
		goto err;
	}
	root_dev = s.st_dev;
	root_ino = s.st_ino;
	if (stat(".", &s)) {
		file = ".";
		goto err;
	}
	pp = path + sizeof(path) - 1;
	*pp = '\0';
	for (pu = up, first = 1;; first = 0) {
		if (root_dev == s.st_dev && root_ino == s.st_ino) {
			*store = '/';
			(void) strcpy(store + 1, pp);
			return (store);
		}
		*pu++ = '.';
		*pu++ = '.';
		*pu = '\0';
		if (!(dir = opendir(up))) {
			file = up;
			goto err;
		}
		*pu++ = '/';
		found = 0;
		file = NULL;
		while (errno = 0, dp = readdir(dir)) {
			if (dp->d_name[0] == '.' && (dp->d_name[1] == '\0' ||
			    dp->d_name[1] == '.' && dp->d_name[2] == '\0'))
				continue;
			bcopy(dp->d_name, pu, dp->d_namlen + 1);
			if (lstat(up, &tmp)) {
				file = dp->d_name;
				save_errno = errno;
				continue;
			}
			if (tmp.st_dev == s.st_dev && tmp.st_ino == s.st_ino) {
				if (!first)
					*--pp = '/';
				pp -= dp->d_namlen;
				bcopy(dp->d_name, pp, dp->d_namlen);
				found = 1;
				break;
			}
		}
		if (errno) {
			save_errno = errno;
			file = up;
		}
		closedir(dir);
		*pu = '\0';
		if (!found) {
			/*
			 * We didn't find the current level in its parent
			 * directory; figure out what to complain about.
			 */
			if (file) {
				errno = save_errno;
				goto err;
			}
			(void) sprintf(store, "%s not found in %s?\n",
				first ? "." : pp, up);
			return ((char *)NULL);
		}

		/* stat "." at current level, then ascend */
		if (lstat(up, &s)) {
			file = up;
			goto err;
		}
	}

err:
	(void) sprintf(store, "getwd: %s: %s", file, strerror(errno));
	return ((char *)NULL);
}

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
static char sccsid[] = "@(#)getcwd.c	5.4 (Berkeley) %G%";
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
	char path[MAXPATHLEN], up[MAXPATHLEN], *strerror();

	if (stat("/", &s))
		goto err;
	root_dev = s.st_dev;
	root_ino = s.st_ino;
	if (stat(".", &s))
		goto err;
	pp = path + sizeof(path) - 1;
	*pp = '\0';
	for (pu = up, first = 1;; first = 0) {
		if (root_dev == s.st_dev && root_ino == s.st_ino) {
			*store = '/';
			(void)strcpy(store + 1, pp);
			return(store);
		}
		*pu++ = '.';
		*pu++ = '.';
		*pu = '\0';
		if (!(dir = opendir(up))) {
			(void)strcpy(path, "getwd: opendir failed.");
			return((char *)NULL);
		}
		*pu++ = '/';
		while (dp = readdir(dir)) {
			if (dp->d_name[0] == '.' && (!dp->d_name[1] ||
			    dp->d_name[1] == '.' && !dp->d_name[2]))
				continue;
			bcopy(dp->d_name, pu, dp->d_namlen + 1);
			if (lstat(up, &tmp))
				goto err;
			if (tmp.st_dev == s.st_dev && tmp.st_ino == s.st_ino) {
				if (!first)
					*--pp = '/';
				pp -= dp->d_namlen;
				bcopy(dp->d_name, pp, dp->d_namlen);
				break;
			}
		}
		closedir(dir);
		*pu = '\0';
		if (lstat(up, &s)) {
err:			(void)sprintf(path, "getwd: %s", strerror(errno));
			return((char *)NULL);
		}
	}
}

/*
 * Copyright (c) 1980 Regents of the University of California.
 * All rights reserved.  The Berkeley software License Agreement
 * specifies the terms and conditions for redistribution.
 */

#if defined(LIBC_SCCS) && !defined(lint)
static char sccsid[] = "@(#)getwd.c	5.2 (Berkeley) 3/9/86";
#endif LIBC_SCCS and not lint

/*
 * getwd() returns the pathname of the current working directory. On error
 * an error message is copied to pathname and null pointer is returned.
 */
#include <sys/param.h>
#include <sys/stat.h>
#include <sys/dir.h>

#define GETWDERR(s)	strcpy(pathname, (s));

char *strcpy();
static int pathsize;			/* pathname length */

char *
getwd(pathname)
	char *pathname;
{
	char pathbuf[MAXPATHLEN];		/* temporary pathname buffer */
	char *pnptr = &pathbuf[(sizeof pathbuf)-1]; /* pathname pointer */
	char curdir[MAXPATHLEN];	/* current directory buffer */
	char *dptr = curdir;		/* directory pointer */
	char *prepend();		/* prepend dirname to pathname */
	dev_t cdev, rdev;		/* current & root device number */
	ino_t cino, rino;		/* current & root inode number */
	DIR *dirp;			/* directory stream */
	struct direct *dir;		/* directory entry struct */
	struct stat d, dd;		/* file status struct */

	pathsize = 0;
	*pnptr = '\0';
	if (stat("/", &d) < 0) {
		GETWDERR("getwd: can't stat /");
		return (NULL);
	}
	rdev = d.st_dev;
	rino = d.st_ino;
	strcpy(dptr, "./");
	dptr += 2;
	if (stat(curdir, &d) < 0) {
		GETWDERR("getwd: can't stat .");
		return (NULL);
	}
	for (;;) {
		if (d.st_ino == rino && d.st_dev == rdev)
			break;		/* reached root directory */
		cino = d.st_ino;
		cdev = d.st_dev;
		strcpy(dptr, "../");
		dptr += 3;
		if ((dirp = opendir(curdir)) == NULL) {
			GETWDERR("getwd: can't open ..");
			return (NULL);
		}
		fstat(dirp->dd_fd, &d);
		if (cdev == d.st_dev) {
			if (cino == d.st_ino) {
				/* reached root directory */
				closedir(dirp);
				break;
			}
			do {
				if ((dir = readdir(dirp)) == NULL) {
					closedir(dirp);
					GETWDERR("getwd: read error in ..");
					return (NULL);
				}
			} while (dir->d_ino != cino);
		} else
			do {
				if ((dir = readdir(dirp)) == NULL) {
					closedir(dirp);
					GETWDERR("getwd: read error in ..");
					return (NULL);
				}
				strcpy(dptr, dir->d_name);
				lstat(curdir, &dd);
			} while(dd.st_ino != cino || dd.st_dev != cdev);
		closedir(dirp);
		pnptr = prepend("/", prepend(dir->d_name, pnptr));
	}
	if (*pnptr == '\0')		/* current dir == root dir */
		strcpy(pathname, "/");
	else
		strcpy(pathname, pnptr);
	return (pathname);
}

/*
 * prepend() tacks a directory name onto the front of a pathname.
 */
static char *
prepend(dirname, pathname)
	register char *dirname;
	register char *pathname;
{
	register int i;			/* directory name size counter */

	for (i = 0; *dirname != '\0'; i++, dirname++)
		continue;
	if ((pathsize += i) < MAXPATHLEN)
		while (i-- > 0)
			*--pathname = *--dirname;
	return (pathname);
}

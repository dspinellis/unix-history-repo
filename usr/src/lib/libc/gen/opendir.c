/* Copyright (c) 1982 Regents of the University of California */

static char sccsid[] = "@(#)opendir.c 4.2 %G%";

#include <sys/types.h>
#include <sys/stat.h>
#include <ndir.h>

/*
 * open a directory.
 */
DIR *
opendir(name)
	char *name;
{
	register DIR *dirp;
	struct stat sbuf;

	dirp = (DIR *)malloc(sizeof(DIR));
	dirp->dd_fd = open(name, 0);
	if (dirp->dd_fd == -1) {
		free(dirp);
		return NULL;
	}
	fstat(dirp->dd_fd, &sbuf);
	if ((sbuf.st_mode & S_IFDIR) == 0) {
		free(dirp);
		return NULL;
	}
	dirp->dd_loc = 0;
	return dirp;
}

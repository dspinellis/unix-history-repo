/* Copyright (c) 1982 Regents of the University of California */

static char sccsid[] = "@(#)closedir.c 4.4 %G%";

#include <sys/param.h>
#include <dir.h>

/*
 * close a directory.
 */
void
closedir(dirp)
	register DIR *dirp;
{
	close(dirp->dd_fd);
	dirp->dd_fd = -1;
	dirp->dd_loc = 0;
	free(dirp);
}

/* Copyright (c) 1982 Regents of the University of California */

static char sccsid[] = "@(#)seekdir.c 4.1 %G%";

#include <sys/types.h>
#include <ndir.h>

/*
 * seek to an entry in a directory.
 * Only values returned by ``telldir'' should be passed to seekdir.
 */
void
seekdir(dirp, loc)
	register DIR *dirp;
	long loc;
{
	lseek(dirp->dd_fd, loc & ~(DIRBLKSIZ - 1), 0);
	dirp->dd_loc = loc % DIRBLKSIZ;
	if (dirp->dd_loc != 0)
		dirp->dd_size = read(dirp->dd_fd, dirp->dd_buf, DIRBLKSIZ);
}

/* Copyright (c) 1982 Regents of the University of California */

static char sccsid[] = "@(#)seekdir.c 1.1 %G%";

#include <sys/types.h>
#include <ndir.h>

/*
 * reset a directory.
 */
void
resetdir(dirp)
	DIR *dirp;
{
	lseek(dirp->dd_fd, (long)0, 0);
	dirp->dd_loc = 0;
}

/* Copyright (c) 1982 Regents of the University of California */

static char sccsid[] = "@(#)closedir.c 4.1 %G%";

#include <sys/types.h>
#include <ndir.h>

/*
 * close a directory.
 */
void
closedir(dirp)
	DIR *dirp;
{
	close(dirp->dd_fd);
	free(dirp);
}

/* Copyright (c) 1982 Regents of the University of California */

static char sccsid[] = "@(#)readdir.c 1.3 %G%";

#include <sys/types.h>
#include <ndir.h>

/*
 * get next entry in a directory.
 */
struct direct *
readdir(dirp)
	register DIR *dirp;
{
	struct direct *dp;

	for (;;) {
		if (dirp->dd_loc == 0) {
			dirp->dd_size = read(dirp->dd_fd, dirp->dd_buf, 
			    MAXDIRSIZ);
			if (dirp->dd_size <= 0)
				return NULL;
		}
		if (dirp->dd_loc >= dirp->dd_size) {
			dirp->dd_loc = 0;
			continue;
		}
		dp = (struct direct *)(dirp->dd_buf + dirp->dd_loc);
		dirp->dd_loc += sizeof(struct direct);
		if (dp->d_ino != 0)
			return dp;
	}
}

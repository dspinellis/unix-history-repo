/* Copyright (c) 1982 Regents of the University of California */

static char sccsid[] = "@(#)readdir.c 1.4 %G%";

#include <sys/types.h>
#include <ndir.h>

/*
 * read an old stlye directory entry and present it as a new one
 */
#ifndef	DIRSIZ
#define	DIRSIZ	14
#endif

struct	olddirect {
	ino_t	d_ino;
	char	d_name[DIRSIZ];
};

/*
 * get next entry in a directory.
 */
struct direct *
readdir(dirp)
	register DIR *dirp;
{
	register struct olddirect *dp;
	static union {
		struct direct un_dir;
		char pad[MAXDIRSIZ];
	} dirun;
#define dir dirun.un_dir

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
		dp = (struct olddirect *)(dirp->dd_buf + dirp->dd_loc);
		dirp->dd_loc += sizeof(struct olddirect);
		if (dp->d_ino == 0)
			continue;
		dir.d_ino = dp->d_ino;
		strncpy(dir.d_name, dp->d_name, DIRSIZ);
		dir.d_namlen = strlen(dir.d_name);
		dir.d_reclen = ((sizeof(struct direct) + dir.d_namlen)
		    + 4) & ~03;
		return (&dir);
	}
}

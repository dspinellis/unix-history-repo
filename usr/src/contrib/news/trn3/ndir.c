/* $Id: ndir.c,v 3.0 1991/09/09 20:23:31 davison Trn $
 */
/* This software is Copyright 1991 by Stan Barber. 
 *
 * Permission is hereby granted to copy, reproduce, redistribute or otherwise
 * use this software as long as: there is no monetary profit gained
 * specifically from the use or reproduction of this software, it is not
 * sold, rented, traded or otherwise marketed, and this copyright notice is
 * included prominently in any copy made. 
 *
 * The author make no claims as to the fitness or correctness of this software
 * for any use whatsoever, and it is provided as is. Any use of this software
 * is at the user's own risk. 
 */

#include "EXTERN.h"
#include "common.h"
#include "INTERN.h"
#include "ndir.h"

#ifdef EMULATE_NDIR
/*
 * support for Berkeley directory reading routine on a V7 file system
 */

/*
 * open a directory.
 */
DIR *
opendir(name)
char *name;
{
	register DIR *dirp;
	register int fd;
	char *malloc();

	if ((fd = open(name, 0)) == -1)
		return NULL;
	if ((dirp = (DIR *)malloc(sizeof(DIR))) == NULL) {
		close (fd);
		return NULL;
	}
	dirp->dd_fd = fd;
	dirp->dd_loc = 0;
	return dirp;
}

/*
 * read an old style directory entry and present it as a new one
 */
#ifndef pyr
#define	ODIRSIZ	14

struct	olddirect {
	ino_t	od_ino;
	char	od_name[ODIRSIZ];
};
#else	/* an Pyramid in the ATT universe */
#define	ODIRSIZ	248

struct	olddirect {
	long	od_ino;
	short	od_fill1, od_fill2;
	char	od_name[ODIRSIZ];
};
#endif

/*
 * get next entry in a directory.
 */
struct direct *
readdir(dirp)
register DIR *dirp;
{
	register struct olddirect *dp;
	static struct direct dir;

	for (;;) {
		if (dirp->dd_loc == 0) {
			dirp->dd_size = read(dirp->dd_fd, dirp->dd_buf,
			    DIRBLKSIZ);
			if (dirp->dd_size <= 0)
				return NULL;
		}
		if (dirp->dd_loc >= dirp->dd_size) {
			dirp->dd_loc = 0;
			continue;
		}
		dp = (struct olddirect *)(dirp->dd_buf + dirp->dd_loc);
		dirp->dd_loc += sizeof(struct olddirect);
		if (dp->od_ino == 0)
			continue;
		dir.d_ino = dp->od_ino;
		strncpy(dir.d_name, dp->od_name, ODIRSIZ);
		dir.d_name[ODIRSIZ] = '\0'; /* insure null termination */
		dir.d_namlen = strlen(dir.d_name);
		dir.d_reclen = DIRSIZ(&dir);
		return (&dir);
	}
}

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

#endif /* EMULATE_NDIR */

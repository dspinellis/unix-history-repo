#include "defs.h"
#if !defined(BSD4_2) && !defined(BSD4_1C)
#include <sys/param.h>
#include "ndir.h"

#ifdef SCCSID
static char	*SccsId = "@(#)ndir.c	1.8	4/26/85";
#endif /* SCCSID */

/*
 * support for Berkeley directory reading routine on a V7 file system
 */

extern char *malloc();

/*
 * open a directory.
 */
DIR *
opendir(name)
char *name;
{
	register DIR *dirp;
	register int fd;

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
#ifdef pyr
/* Pyramid in the AT&T universe */
#define ODIRSIZ 248
struct olddirect {
	long	od_ino;
	short	od_fill1, od_fill2;
	char od_name[ODIRSIZ];
};
#else /* V7 file system */
#define	ODIRSIZ	14

struct	olddirect {
	short	od_ino;
	char	od_name[ODIRSIZ];
};
#endif /* V7 */

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
	free((char *)dirp);
}
#endif /* !BSD4_2 && !BSD4_1C */

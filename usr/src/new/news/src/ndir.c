#include "defs.h"
#if !defined(BSD4_2) && !defined(BSD4_1C) && !defined(HP9K5)
#ifdef M_XENIX
#include <sys/types.h>
#endif /* M_XENIX */
#include <sys/param.h>
#include "ndir.h"

#ifdef SCCSID
static char	*SccsId = "@(#)ndir.c	1.12	10/15/87";
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
#endif /* !pyr */

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

/*
 * seek to an entry in a directory.
 * Only values returned by "telldir" should be passed to seekdir.
 */
void
seekdir(dirp, loc)
register DIR *dirp;
long loc;
{
	long curloc, base, offset;
	struct direct *dp;
	long lseek(), telldir();

	curloc = telldir(dirp);
	if (loc == curloc)
		return;
	base = loc & ~(DIRBLKSIZ - 1);
	offset = loc & (DIRBLKSIZ - 1);
	(void) lseek(dirp->dd_fd, base, 0);
	dirp->dd_loc = 0;
	while (dirp->dd_loc < offset) {
		dp = readdir(dirp);
		if (dp == NULL)
			return;
	}
}

/*
 * return a pointer into a directory
 */
long
telldir(dirp)
DIR *dirp;
{
	return lseek(dirp->dd_fd, 0L, 1) - dirp->dd_size + dirp->dd_loc;
}
#endif /* !BSD4_2 && !BSD4_1C && !HP9K5 */

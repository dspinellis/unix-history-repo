/*************************************************************************
 * This program is copyright (C) 1985, 1986 by Jonathan Payne.  It is    *
 * provided to you without charge for use only on a licensed Unix        *
 * system.  You may copy JOVE provided that this notice is included with *
 * the copy.  You may not sell copies of this program or versions        *
 * modified for use on microcomputer systems, unless the copies are      *
 * included with a Unix system distribution and the source is provided.  *
 *************************************************************************/

#include "jove.h"
#include <sys/stat.h>
#include <sys/dir.h>

#ifdef F_COMPLETION

#ifdef BSD4_2

#define DIRSIZE(entry)	DIRSIZ(entry)

#else

#define DIRSIZE(entry)	(min(strlen(entry->d_name), DIRSIZ))

typedef struct {
	int	d_fd;		/* File descriptor for this directory */
} DIR;

DIR *
opendir(dir)
char	*dir;
{
	DIR	*dp = (DIR *) malloc(sizeof *dp);
	struct stat	stbuf;

	if ((dp->d_fd = open(dir, 0)) == -1)
		return 0;
	if ((fstat(dp->d_fd, &stbuf) == -1) || !(stbuf.st_mode & S_IFDIR)) {
		closedir(dp);
		return 0;	/* this isn't a directory! */
	}
	return dp;
}

closedir(dp)
DIR	*dp;
{
	(void) close(dp->d_fd);
	free((char *) dp);
}

struct direct *
readdir(dp)
DIR	*dp;
{
	static struct direct	dir;

	do
		if (read(dp->d_fd, &dir, sizeof dir) != sizeof dir)
			return 0;
	while (dir.d_ino == 0);

	return &dir;
}

#endif BSD4_2

/* Scandir returns the number of entries or -1 if the directory cannoot
   be opened or malloc fails. */

scandir(dir, nmptr, qualify, sorter)
char	*dir;
char	***nmptr;
int	(*qualify)();
int	(*sorter)();
{
	DIR	*dirp;
	struct direct	*entry;
	char	**ourarray;
	unsigned int	nalloc = 10,
			nentries = 0;

	if ((dirp = opendir(dir)) == 0)
		return -1;
	if ((ourarray = (char **) malloc(nalloc * sizeof (char *))) == 0)
memfail:	complain("[Malloc failed: cannot scandir]");
	while ((entry = readdir(dirp)) != 0) {
		if (qualify != 0 && (*qualify)(entry->d_name) == 0)
			continue;
		if (nentries == nalloc) {
			ourarray = (char **) realloc((char *) ourarray, (nalloc += 10) * sizeof (char *));
			if (ourarray == 0)
				goto memfail;
		}
		ourarray[nentries] = (char *) malloc(DIRSIZE(entry) + 1);
		null_ncpy(ourarray[nentries], entry->d_name, (int) DIRSIZE(entry));
		nentries++;
	}
	closedir(dirp);
	if ((nentries + 1) != nalloc)
		ourarray = (char **) realloc((char *) ourarray,
					((nentries + 1) * sizeof (char *)));
	if (sorter != 0)
		qsort((char *) ourarray, nentries, sizeof (char **), sorter);
	*nmptr = ourarray;
	ourarray[nentries] = 0;		/* guaranteed 0 pointer */

	return nentries;
}

freedir(nmptr, nentries)
char	***nmptr;
{
	char	**ourarray = *nmptr;

	while (--nentries >= 0)
		free(*ourarray++);
	free((char *) *nmptr);
	*nmptr = 0;
}

alphacomp(a, b)
char	**a,
	**b;
{
	return strcmp(*a, *b);
}

#endif

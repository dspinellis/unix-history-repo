/***************************************************************************
 * This program is Copyright (C) 1986, 1987, 1988 by Jonathan Payne.  JOVE *
 * is provided to you without charge, and with no warranty.  You may give  *
 * away copies of JOVE, including sources, provided that this notice is    *
 * included in all the files.                                              *
 ***************************************************************************/

#include "jove.h"

#ifdef F_COMPLETION

#ifdef MSDOS
# include <dos.h>
# include <search.h>
#endif

#ifdef UNIX
# include <sys/stat.h>
# ifdef M_XENIX
#  include <sys/ndir.h>
# else
#  include <sys/dir.h>
# endif /* M_XENIX */
#endif

#ifdef UNIX

#if defined(BSD4_2) || defined(M_XENIX)
# define DIRSIZE(entry)	DIRSIZ(entry)
#else
# define DIRSIZE(entry)	(entry->d_name[DIRSIZ-1]=='\0' ? strlen(entry->d_name) : DIRSIZ)

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
#if defined(elxsi) && defined(SYSV)
	/*
	 * Elxsi has a BSD4.2 implementation which may or may not use
	 * `twisted inodes' ...  Anyone able to check?
	 */
	while (*(unsigned short *)&dir.d_ino == 0);
#else
	while (dir.d_ino == 0);
#endif

	return &dir;
}

#endif /* BSD4_2 */

/* Scandir returns the number of entries or -1 if the directory cannoot
   be opened or malloc fails. */

int
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
		nentries += 1;
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

#endif /* UNIX */

#ifdef MSDOS
# define DIRSIZ	13
# define DIRSIZE(entry)	strlen(entry.name)

/* Scandir returns the number of entries or -1 if the directory cannoot
   be opened or malloc fails. */

unsigned int fmask = _A_NORMAL|_A_RDONLY|_A_HIDDEN|_A_SUBDIR;

int
scandir(dir, nmptr, qualify, sorter)
char	*dir;
char	***nmptr;
int	(*qualify)();
int	(*sorter)();
{
	char dirname[FILESIZE];
	struct find_t entry;
	char *ptr;
	char	**ourarray;
	unsigned int	nalloc = 10,
			nentries = 0;

	strcpy(dirname, dir);
	ptr = &dirname[strlen(dirname)-1];
	if ((dirname[1] == ':' && !dirname[2]) || (*ptr == '/') || (*ptr == '\\'))
	   strcat(dirname, "*.*");
	else
	   strcat(dirname, "/*.*");

	if (_dos_findfirst(dirname, fmask, &entry))
	   return -1;
	if ((ourarray = (char **) malloc(nalloc * sizeof (char *))) == 0)
memfail:	complain("[Malloc failed: cannot scandir]");
	do  {
		if ((fmask == 0x10) && !(entry.attrib&fmask))
			goto skip;
		strlwr(entry.name);
		if (qualify != (int (*)())0 && (*qualify)(entry.name) == 0)
			goto skip;
		if (nentries == nalloc) {
			ourarray = (char **) realloc((char *) ourarray, (nalloc += 10) * sizeof (char *));
			if (ourarray == 0)
				goto memfail;
		}
		ourarray[nentries] = (char *) malloc(DIRSIZE(entry) + 1);
		null_ncpy(ourarray[nentries], entry.name, (int) DIRSIZE(entry));
		nentries++;
skip:	;
    }
    while (_dos_findnext(&entry) == 0);

	if ((nentries + 1) != nalloc)
		ourarray = (char **) realloc((char *) ourarray,
					((nentries + 1) * sizeof (char *)));
	if (sorter != (int (*)())0)
		qsort((char *) ourarray, nentries, sizeof (char **), sorter);
	*nmptr = ourarray;
	ourarray[nentries] = 0;		/* guaranteed 0 pointer */

	return nentries;
}

#endif /* MSDOS */

void
freedir(nmptr, nentries)
char	***nmptr;
{
	char	**ourarray = *nmptr;

	while (--nentries >= 0)
		free(*ourarray++);
	free((char *) *nmptr);
	*nmptr = 0;
}

int
alphacomp(a, b)
char	**a,
	**b;
{
	return strcmp(*a, *b);
}
#endif

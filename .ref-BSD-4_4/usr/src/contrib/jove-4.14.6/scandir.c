/***************************************************************************
 * This program is Copyright (C) 1986, 1987, 1988 by Jonathan Payne.  JOVE *
 * is provided to you without charge, and with no warranty.  You may give  *
 * away copies of JOVE, including sources, provided that this notice is    *
 * included in all the files.                                              *
 ***************************************************************************/

/*
 * This file is used as a compiled module by Jove and also included as
 * source in recover.c
 */
#ifndef TUNED
# include "jove.h"
#endif
#include "scandir.h"

#ifdef	F_COMPLETION

#ifdef	MSDOS
# include <dos.h>
# include <search.h>
#endif

#ifdef	UNIX
# include <sys/stat.h>
# ifdef	M_XENIX
#  include <sys/ndir.h>
#  ifndef dirent
#   define dirent direct
#  endif
# else
#  ifdef	DIRENT
#   include <dirent.h>
#  else
#   include <sys/dir.h>
#   ifndef dirent
#    define dirent direct
#   endif
#  endif	DIRENT
# endif	/* M_XENIX */
#endif

#ifdef	UNIX

#define DIRSIZE(entry)	(strlen((entry)->d_name))

#if !defined(BSD_DIR) && !defined(DIRENT)

typedef struct {
	int	d_fd;		/* File descriptor for this directory */
} DIR;

private int
closedir(dp)
DIR	*dp;
{
	(void) close(dp->d_fd);
	free((char *) dp);
	return 0;   /* don't know how to fail */
}

private DIR *
opendir(dir)
char	*dir;
{
	DIR	*dp = (DIR *) emalloc(sizeof *dp);
	struct stat	stbuf;

	if ((dp->d_fd = open(dir, 0)) == -1)
		return NULL;
	if ((fstat(dp->d_fd, &stbuf) == -1) || !(stbuf.st_mode & S_IFDIR)) {
		closedir(dp);
		return NULL;	/* this isn't a directory! */
	}
	return dp;
}

private dirent *
readdir(dp)
DIR	*dp;
{
	static dirent	dir;

	do {
		if (read(dp->d_fd, (UnivPtr) &dir, sizeof dir) != sizeof dir)
			return NULL;
#if	defined(elxsi) && defined(SYSV)
	/*
	 * Elxsi has a BSD4.2 implementation which may or may not use
	 * `twisted inodes' ...  Anyone able to check?
	 */
	} while (*(unsigned short *)&dir.d_ino == 0);
#else
	} while (dir.d_ino == 0);
#endif

	return &dir;
}

#endif	/* ! BSD_DIR */

/* Scandir returns the number of entries or -1 if the directory cannoot
   be opened or malloc fails. */

int
jscandir(dir, nmptr, qualify, sorter)
char	*dir;
char	***nmptr;
int	(*qualify) proto((char *));
int	(*sorter) proto((UnivConstPtr, UnivConstPtr));
{
	DIR	*dirp;
	struct  dirent	*entry;
	char	**ourarray;
	unsigned int	nalloc = 10,
			nentries = 0;

	if ((dirp = opendir(dir)) == NULL)
		return -1;
	ourarray = (char **) emalloc(nalloc * sizeof (char *));
	while ((entry = readdir(dirp)) != NULL) {
		if (qualify != NULL && (*qualify)(entry->d_name) == 0)
			continue;
		if (nentries == nalloc) {
			ourarray = (char **) realloc((UnivPtr) ourarray, (nalloc += 10) * sizeof (char *));
			if (ourarray == NULL)
				complain("[Malloc failed: cannot scandir]");
		}
		ourarray[nentries] = (char *) emalloc(DIRSIZE(entry) + 1);
		null_ncpy(ourarray[nentries], entry->d_name, (size_t) DIRSIZE(entry));
		nentries += 1;
	}
	closedir(dirp);
	if ((nentries + 1) != nalloc)
		ourarray = (char **) erealloc((UnivPtr) ourarray,
					((nentries + 1) * sizeof (char *)));
	if (sorter != NULL)
		qsort((UnivPtr) ourarray, nentries, sizeof (char **), sorter);
	*nmptr = ourarray;
	ourarray[nentries] = NULL;		/* guaranteed NULL pointer */

	return nentries;
}

#endif	/* UNIX */

#ifdef	MSDOS
# define DIRSIZE(entry)	strlen((entry).name)

/* Scandir returns the number of entries or -1 if the directory cannot
   be opened or malloc fails. */

unsigned int fmask = _A_NORMAL|_A_RDONLY|_A_HIDDEN|_A_SUBDIR;

int
jscandir(dir, nmptr, qualify, sorter)
char	*dir;
char	***nmptr;
int	(*qualify) proto((char *));
int	(*sorter) proto((UnivConstPtr, UnivConstPtr));
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
	if ((ourarray = (char **) emalloc(nalloc * sizeof (char *))) == NULL)
	do  {
		if ((fmask == 0x10) && !(entry.attrib&fmask))
			continue;
		strlwr(entry.name);
		if (qualify != NULL && (*qualify)(entry.name) == 0)
			continue;
		if (nentries == nalloc) {
			ourarray = (char **) realloc((char *) ourarray, (nalloc += 10) * sizeof (char *));
			if (ourarray == NULL)
				complain("[Malloc failed: cannot scandir]");
		}
		ourarray[nentries] = (char *) emalloc(DIRSIZE(entry) + 1);
		null_ncpy(ourarray[nentries], entry.name, (int) DIRSIZE(entry));
		nentries++;
	} while (_dos_findnext(&entry) == 0);

	if ((nentries + 1) != nalloc)
		ourarray = (char **) erealloc((char *) ourarray,
					((nentries + 1) * sizeof (char *)));
	if (sorter != (int (*)())NULL)
		qsort((char *) ourarray, nentries, sizeof (char **), sorter);
	*nmptr = ourarray;
	ourarray[nentries] = NULL;		/* guaranteed NULL pointer */

	return nentries;
}

#endif	/* MSDOS */

void
freedir(nmptr, nentries)
char	***nmptr;
int	nentries;
{
	char	**ourarray = *nmptr;

	while (--nentries >= 0)
		free((UnivPtr) *ourarray++);
	free((UnivPtr) *nmptr);
	*nmptr = NULL;
}

int
alphacomp(a, b)
UnivConstPtr	a,
	b;
{
	return strcmp(*(const char **)a, *(const char **)b);
}
#endif /* F_COMPLETION */

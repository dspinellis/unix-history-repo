/*
 * getwd() returns the pathname of the current working directory. On error
 * an error message is copied to pathname and null pointer is returned.
 *
 * A version of this function was copied to the standard C library. It is
 * only included here for compatibility with non-BSD UNIX systems.
 */
#include <sys/param.h>
#include <sys/stat.h>
#include <sys/dir.h>
#include "null.h"
#include "path.h"

#define GETWDERR(s)	strcpy(pathname, (s));

char *strcpy();				/* string copy */

static int pathsize;			/* pathname length */

char *
getwd(pathname)
	char *pathname;
{
	char pathbuf[PATHSIZE];		/* temporary pathname buffer */
	char *pnptr = &pathbuf[(sizeof pathbuf)-1]; /* pathname pointer */
	char *prepend();		/* prepend dirname to pathname */
	dev_t rdev;			/* root device number */
	DIR *dirp;			/* directory stream */
	ino_t rino;			/* root inode number */
	struct direct *dir;		/* directory entry struct */
	struct stat d ,dd;		/* file status struct */

	pathsize = 0;
	*pnptr = '\0';
	stat(ROOTDIR, &d);
	rdev = d.st_dev;
	rino = d.st_ino;
	for (;;)
		{
		stat(CURDIR, &d);
		if (d.st_ino == rino && d.st_dev == rdev)
			break;		/* reached root directory */
		if ((dirp = opendir(PARENTDIR)) == NULL)
			{
			GETWDERR("getwd: can't open ..");
			goto fail;
			}
		if (chdir(PARENTDIR) < 0)
			{
			GETWDERR("getwd: can't chdir to ..");
			goto fail;
			}
		fstat(dirp->dd_fd, &dd);
		if(d.st_dev == dd.st_dev)
			{
			if(d.st_ino == dd.st_ino)
				{	/* reached root directory */
				closedir(dirp);
				break;
				}
			do	{
				if ((dir = readdir(dirp)) == NULL)
					{
					closedir(dirp);
					GETWDERR("getwd: read error in ..");
					goto fail;
					}
				} while (dir->d_ino != d.st_ino);
			}
		else do	{
			if((dir = readdir(dirp)) == NULL)
				{
				closedir(dirp);
				GETWDERR("getwd: read error in ..");
				goto fail;
				}
			stat(dir->d_name, &dd);
			} while(dd.st_ino != d.st_ino || dd.st_dev != d.st_dev);
		closedir(dirp);
		pnptr = prepend(PATHSEP, prepend(dir->d_name, pnptr));
		}
	if (*pnptr == '\0')
		{			/* current dir == root dir */
		strcpy(pathname, ROOTDIR);
		}
	else	{
		strcpy(pathname, pnptr);
		if (chdir(pnptr) < 0)
			{
			GETWDERR("getwd: can't change back to .");
			return(NULL);
			}
		}
	return(pathname);

fail:	chdir(prepend(CURDIR, pnptr));
	return(NULL);
}



/*
 * prepend() tacks a directory name onto the front of a pathname.
 */
static char *
prepend(dirname, pathname)
	register char *dirname;
	register char *pathname;
{
	register int i;			/* directory name size counter */

	for (i = 0; *dirname != '\0'; i++, dirname++)
		continue;
	if ((pathsize += i) < PATHSIZE)
		while (i-- > 0)
			*--pathname = *--dirname;
	return(pathname);
}

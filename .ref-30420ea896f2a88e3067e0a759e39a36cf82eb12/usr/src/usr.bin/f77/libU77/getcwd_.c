/*
char id_getcwd[] = "@(#)getcwd_.c	1.5";
 * Get pathname of current working directory.
 *
 * calling sequence:
 *	character*128 path
 *	ierr = getcwd(path)
 * where:
 *	path will receive the pathname of the current working directory.
 *	ierr will be 0 if successful, a system error code otherwise.
 */

#include <sys/param.h>
#ifndef	MAXPATHLEN
#define MAXPATHLEN	128
#endif

extern int errno;
char	*getwd();

long
getcwd_(path, len)
char *path;
long len;
{
	char	*p;
	char	pathname[MAXPATHLEN];

	p = getwd(pathname);
	b_char(pathname, path, len);
	if (p)
		return(0L);
	else
		return((long)errno);
}

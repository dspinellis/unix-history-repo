/*
char id_getcwd[] = "@(#)getcwd_.c	1.4";
 * Get pathname of current working directory.
 *
 * calling sequence:
 *	character*128 path
 *	ierr = getcwd(path)
 * where:
 *	path will receive the pathname of the current working directory.
 *	ierr will be 0 if successful, a system error code otherwise.
 */

extern int errno;
char	*getwd();

long
getcwd_(path, len)
char *path;
long len;
{
	char	*p;
	char	pathname[1024];

	p = getwd(pathname);
	b_char(pathname, path, len);
	if (p)
		return(0L);
	else
		return((long)errno);
}

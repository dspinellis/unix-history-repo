/*
 * Copyright (c) 1980 Regents of the University of California.
 * All rights reserved.  The Berkeley software License Agreement
 * specifies the terms and conditions for redistribution.
 *
 *	@(#)getcwd_.c	5.1	%G%
 */

/*
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

/*
 * Copyright (c) 1983 Regents of the University of California.
 * All rights reserved.
 *
 * %sccs.include.redist.c%
 */

#if defined(LIBC_SCCS) && !defined(lint)
static char sccsid[] = "@(#)seekdir.c	5.7 (Berkeley) %G%";
#endif /* LIBC_SCCS and not lint */

#include <sys/param.h>
#include <dirent.h>

/*
 * Seek to an entry in a directory.
 * _seekdir is in telldir.c so that it can share opaque data structures.
 */
void
seekdir(dirp, loc)
	DIR *dirp;
	long loc;
{

	_seekdir(dirp, loc);
}

/*-
 * Copyright (c) 1990 The Regents of the University of California.
 * All rights reserved.
 *
 * %sccs.include.redist.c%
 */

#if defined(LIBC_SCCS) && !defined(lint)
static char sccsid[] = "@(#)rewinddir.c	5.1 (Berkeley) %G%";
#endif /* LIBC_SCCS and not lint */

#include <sys/types.h>
#include <dirent.h>

void
rewinddir(dirp)
	DIR *dirp;
{
	extern long _rewinddir;

	_seekdir((dirp), _rewinddir);
	_rewinddir = telldir(dirp);
}

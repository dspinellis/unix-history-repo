/*
 * Copyright (c) 1989 The Regents of the University of California.
 * All rights reserved.
 *
 * %sccs.include.redist.c%
 */

#if defined(LIBC_SCCS) && !defined(lint)
static char sccsid[] = "@(#)creat.c	5.3 (Berkeley) %G%";
#endif /* LIBC_SCCS and not lint */

#include <fcntl.h>

#if __STDC__
creat(const char *path, mode_t mode)
#else
creat(path, mode)
	char *path;
	mode_t mode;
#endif
{
	return(open(path, O_WRONLY|O_CREAT|O_TRUNC, mode));
}

/*-
 * Copyright (c) 1990 The Regents of the University of California.
 * All rights reserved.
 *
 * %sccs.include.redist.c%
 */

#if defined(LIBC_SCCS) && !defined(lint)
static char sccsid[] = "@(#)utime.c	5.3 (Berkeley) %G%";
#endif /* LIBC_SCCS and not lint */

#include <sys/time.h>
#include <utime.h>

utime(path, times)
	char *path;
	struct utimbuf *times;
{
	struct timeval tv[2];

	tv[0].tv_sec = times->actime;
	tv[1].tv_sec = times->modtime;
	tv[0].tv_usec = tv[1].tv_usec = 0;
	return(utimes(path, tv));
}

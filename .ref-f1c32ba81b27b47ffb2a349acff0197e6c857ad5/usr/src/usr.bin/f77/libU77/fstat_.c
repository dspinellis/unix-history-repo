/*-
 * Copyright (c) 1980 The Regents of the University of California.
 * All rights reserved.
 *
 * %sccs.include.proprietary.c%
 */

#ifndef lint
static char sccsid[] = "@(#)fstat_.c	5.2 (Berkeley) %G%";
#endif /* not lint */

/*
 * get file status
 *
 * calling sequence:
 *	integer fstat, statb(12)
 *	call fstat (name, statb)
 * where:
 *	'statb' will receive the stat structure for file 'name'.
 */

#include <sys/types.h>
#include <sys/stat.h>
#include "../libI77/f_errno.h"
#include "../libI77/fiodefs.h"

extern unit units[];

long fstat_(lunit, stbuf)
long *lunit, *stbuf;
{
	struct stat statb;

	if (*lunit < 0 || *lunit >= MXUNIT)
		return((long)(errno=F_ERUNIT));
	if (!units[*lunit].ufd)
		return((long)(errno=F_ERNOPEN));
	if (fstat(fileno(units[*lunit].ufd), &statb) == 0)
	{
		*stbuf++ = statb.st_dev;
		*stbuf++ = statb.st_ino;
		*stbuf++ = statb.st_mode;
		*stbuf++ = statb.st_nlink;
		*stbuf++ = statb.st_uid;
		*stbuf++ = statb.st_gid;
		*stbuf++ = statb.st_rdev;
		*stbuf++ = statb.st_size;
		*stbuf++ = statb.st_atime;
		*stbuf++ = statb.st_mtime;
		*stbuf++ = statb.st_ctime;
		*stbuf++ = statb.st_blksize;
		return(0L);
	}
	return ((long)errno);
}

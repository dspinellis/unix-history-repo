/*-
 * Copyright (c) 1982, 1988, 1993
 *	The Regents of the University of California.  All rights reserved.
 *
 * This module is believed to contain source code proprietary to AT&T.
 * Use and redistribution is subject to the Berkeley Software License
 * Agreement and your Software Agreement with AT&T (Western Electric).
 *
 *	@(#)close.c	8.1 (Berkeley) 6/11/93
 */

#include <sys/param.h>
#include <stand.att/saio.h>

close(fdesc)
	int fdesc;
{
#ifndef SMALL
	struct iob *file;

	fdesc -= 3;
	if (fdesc < 0 || fdesc >= SOPEN_MAX ||
	    ((file = &iob[fdesc])->i_flgs&F_ALLOC) == 0) {
		errno = EBADF;
		return (-1);
	}
	if ((file->i_flgs&F_FILE) == 0)
		devclose(file);
	file->i_flgs = 0;
#endif
	return (0);
}

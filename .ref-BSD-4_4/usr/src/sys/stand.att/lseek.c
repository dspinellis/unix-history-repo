/*-
 * Copyright (c) 1982, 1988, 1993
 *	The Regents of the University of California.  All rights reserved.
 *
 * This module is believed to contain source code proprietary to AT&T.
 * Use and redistribution is subject to the Berkeley Software License
 * Agreement and your Software Agreement with AT&T (Western Electric).
 *
 *	@(#)lseek.c	8.1 (Berkeley) 6/11/93
 */

#include <sys/param.h>
#include <stand.att/saio.h>

off_t
lseek(fdesc, addr, ptr)
	int fdesc, ptr;
	off_t addr;
{
	register struct iob *io;

#ifndef SMALL
	if (ptr != L_SET) {
		printf("Seek not from beginning of file\n");
		errno = EOFFSET;
		return (-1);
	}
#endif
	fdesc -= 3;
#ifndef SMALL
	if (fdesc < 0 || fdesc >= SOPEN_MAX ||
	    ((io = &iob[fdesc])->i_flgs & F_ALLOC) == 0) {
		errno = EBADF;
		return (-1);
	}
#endif
	io->i_offset = addr;
	io->i_bn = addr / DEV_BSIZE;
	io->i_cc = 0;
	return (0);
}

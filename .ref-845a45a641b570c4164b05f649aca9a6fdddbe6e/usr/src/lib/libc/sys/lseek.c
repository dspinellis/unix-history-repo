/*
 * Copyright (c) 1992 The Regents of the University of California.
 * All rights reserved.
 *
 * %sccs.include.redist.c%
 */

#if defined(LIBC_SCCS) && !defined(lint)
static char sccsid[] = "@(#)lseek.c	8.1 (Berkeley) %G%";
#endif /* LIBC_SCCS and not lint */

#include <sys/types.h>
#include <sys/syscall.h>

/*
 * This function provides 64-bit offset padding that
 * is not supplied by GCC 1.X but is supplied by GCC 2.X.
 */
off_t
lseek(fd, offset, whence)
	int	fd;
	off_t	offset;
	int	whence;
{
	extern off_t __indir();

	return(__indir((quad_t)SYS_lseek, fd, 0, offset, whence));
}

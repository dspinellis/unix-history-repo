/*
 * Copyright (c) 1992 The Regents of the University of California.
 * All rights reserved.
 *
 * %sccs.include.redist.c%
 */

#if defined(LIBC_SCCS) && !defined(lint)
static char sccsid[] = "@(#)mmap.c	8.1 (Berkeley) %G%";
#endif /* LIBC_SCCS and not lint */

#include <sys/types.h>
#include <sys/mman.h>
#include <sys/syscall.h>

/*
 * This function provides 64-bit offset padding that
 * is not supplied by GCC 1.X but is supplied by GCC 2.X.
 */
caddr_t
mmap(addr, len, prot, flags, fd, offset)
	caddr_t addr;
	size_t	len;
	int	prot;
	int	flags;
	int	fd;
	off_t	offset;
{

	return((caddr_t)__indir((quad_t)SYS_mmap, addr, len, prot, flags, fd, 0,
		offset));
}

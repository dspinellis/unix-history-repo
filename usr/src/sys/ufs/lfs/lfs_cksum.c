/*-
 * Copyright (c) 1991 The Regents of the University of California.
 * All rights reserved.
 *
 * %sccs.include.redist.c%
 *
 *	@(#)lfs_cksum.c	5.1 (Berkeley) %G%
 */

#include <sys/types.h>

/*
 * cksum --
 *	Simple, general purpose, fast checksum.
 */
u_long
cksum(str, len)
	register void *str;
	register size_t len;
{
	register u_long sum;
	
	len &= ~(sizeof(u_short) - 1);
	for (sum = 0; len; len -= sizeof(u_short))
		sum ^= *((u_short *)str)++;
	return (sum);
}

/*-
 * Copyright (c) 1990 The Regents of the University of California.
 * All rights reserved.
 *
 * This code is derived from software contributed to Berkeley by
 * Chris Torek.
 *
 * %sccs.include.redist.c%
 */

#if defined(LIBC_SCCS) && !defined(lint)
static char sccsid[] = "@(#)memcpy.c	5.6 (Berkeley) %G%";
#endif /* LIBC_SCCS and not lint */

#include <string.h>
#include <sys/stdc.h>

/*
 * Copy a block of memory.
 */
void *
memcpy(dst, src, n)
	void *dst;
	const void *src;
	size_t n;
{
	bcopy((const char *)src, (char *)dst, n);
	return(dst);
}

/*-
 * Copyright (c) 1990, 1993
 *	The Regents of the University of California.  All rights reserved.
 *
 * This code is derived from software contributed to Berkeley by
 * Chris Torek.
 *
 * %sccs.include.redist.c%
 */

#if defined(LIBC_SCCS) && !defined(lint)
static char sccsid[] = "@(#)puts.c	8.1 (Berkeley) %G%";
#endif /* LIBC_SCCS and not lint */

#include <stdio.h>
#include <string.h>
#include "fvwrite.h"

/*
 * Write the given string to stdout, appending a newline.
 */
puts(s)
	char const *s;
{
	size_t c = strlen(s);
	struct __suio uio;
	struct __siov iov[2];

	iov[0].iov_base = (void *)s;
	iov[0].iov_len = c;
	iov[1].iov_base = "\n";
	iov[1].iov_len = 1;
	uio.uio_resid = c + 1;
	uio.uio_iov = &iov[0];
	uio.uio_iovcnt = 2;
	return (__sfvwrite(stdout, &uio) ? EOF : '\n');
}

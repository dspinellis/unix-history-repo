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
static char sccsid[] = "@(#)fputs.c	8.1 (Berkeley) %G%";
#endif /* LIBC_SCCS and not lint */

#include <stdio.h>
#include <string.h>
#include "fvwrite.h"

/*
 * Write the given string to the given file.
 */
fputs(s, fp)
	const char *s;
	FILE *fp;
{
	struct __suio uio;
	struct __siov iov;

	iov.iov_base = (void *)s;
	iov.iov_len = uio.uio_resid = strlen(s);
	uio.uio_iov = &iov;
	uio.uio_iovcnt = 1;
	return (__sfvwrite(fp, &uio));
}

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
static char sccsid[] = "@(#)putw.c	8.1 (Berkeley) %G%";
#endif /* LIBC_SCCS and not lint */

#include <stdio.h>
#include "fvwrite.h"

putw(w, fp)
	int w;
	FILE *fp;
{
	struct __suio uio;
	struct __siov iov;

	iov.iov_base = &w;
	iov.iov_len = uio.uio_resid = sizeof(w);
	uio.uio_iov = &iov;
	uio.uio_iovcnt = 1;
	return (__sfvwrite(fp, &uio));
}

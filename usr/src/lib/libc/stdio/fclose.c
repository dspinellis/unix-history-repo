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
static char sccsid[] = "@(#)fclose.c	5.3 (Berkeley) %G%";
#endif /* LIBC_SCCS and not lint */

#include <errno.h>
#include <stdio.h>
#include <stdlib.h>
#include "local.h"

fclose(fp)
	register FILE *fp;
{
	register int r;

	if (fp->_flags == 0) {	/* not open! */
		errno = EBADF;
		return (EOF);
	}
	r = fp->_flags & __SWR ? __sflush(fp) : 0;
	if (fp->_close != NULL && (*fp->_close)(fp->_cookie) < 0)
		r = EOF;
	if (fp->_flags & __SMBF)
		free((char *)fp->_bf._base);
	if (HASUB(fp))
		FREEUB(fp);
	if (HASLB(fp))
		FREELB(fp);
	fp->_flags = 0;		/* Release this FILE for reuse. */
	fp->_r = fp->_w = 0;	/* Mess up if reaccessed. */
	return (r);
}

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
static char sccsid[] = "@(#)setbuf.c	8.1 (Berkeley) %G%";
#endif /* LIBC_SCCS and not lint */

#include <stdio.h>
#include "local.h"

void
setbuf(fp, buf)
	FILE *fp;
	char *buf;
{
	(void) setvbuf(fp, buf, buf ? _IOFBF : _IONBF, BUFSIZ);
}

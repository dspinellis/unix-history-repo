/*-
 * Copyright (c) 1991, 1993
 *	The Regents of the University of California.  All rights reserved.
 *
 * %sccs.include.redist.c%
 */

#ifndef lint
static char sccsid[] = "@(#)misc.c	8.2 (Berkeley) %G%";
#endif /* not lint */

#include <sys/types.h>

#include <err.h>
#include <errno.h>
#include <stdio.h>
#include <stdlib.h>
#include <string.h>
#include <unistd.h>
#include "extern.h"

void
get(fd, off, p, len)
	int fd;
	off_t off;
	void *p;
	size_t len;
{
	int rbytes;

	if (lseek(fd, off, SEEK_SET) < 0)
		err(1, "%s", special);
	if ((rbytes = read(fd, p, len)) < 0)
		err(1, "%s", special);
	if (rbytes != len)
		errx(1, "%s: short read (%d, not %d)", special, rbytes, len);
}

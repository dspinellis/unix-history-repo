/*-
 * Copyright (c) 1991 The Regents of the University of California.
 * All rights reserved.
 *
 * %sccs.include.redist.c%
 */

#ifndef lint
static char sccsid[] = "@(#)print.c	5.1 (Berkeley) %G%";
#endif /* not lint */

#include <sys/types.h>
#include <stdio.h>
#include "extern.h"

void
pcrc(fn, val, len)
	char *fn;
	u_long val, len;
{
	(void)printf("%lu %lu %s\n", val, len, fn);
}

void
psum1(fn, val, len)
	char *fn;
	u_long val, len;
{
	(void)printf("%lu %lu %s\n", val, (len + 1023) / 1024, fn);
}

void
psum2(fn, val, len)
	char *fn;
	u_long val, len;
{
	(void)printf("%lu %lu %s\n", val, (len + 511) / 512, fn);
}

/*-
 * Copyright (c) 1991 The Regents of the University of California.
 * All rights reserved.
 *
 * %sccs.include.redist.c%
 */

#ifndef lint
static char sccsid[] = "@(#)print.c	5.2 (Berkeley) %G%";
#endif /* not lint */

#include <sys/types.h>
#include <stdio.h>
#include "extern.h"

void
pcrc(fn, val, len)
	char *fn;
	u_long val, len;
{
	(void)printf("%lu %lu", val, len);
	if (fn)
		(void)printf(" %s", fn);
	(void)printf("\n");
}

void
psum1(fn, val, len)
	char *fn;
	u_long val, len;
{
	(void)printf("%lu %lu", val, (len + 1023) / 1024);
	if (fn)
		(void)printf(" %s", fn);
	(void)printf("\n");
}

void
psum2(fn, val, len)
	char *fn;
	u_long val, len;
{
	(void)printf("%lu %lu", val, (len + 511) / 512);
	if (fn)
		(void)printf(" %s", fn);
	(void)printf("\n");
}

/*-
 * Copyright (c) 1990, 1993
 *	The Regents of the University of California.  All rights reserved.
 *
 * This code is derived from software contributed to Berkeley by
 * William Jolitz.
 *
 * %sccs.include.redist.c%
 *
 *	@(#)confxx.c	8.1 (Berkeley) %G%
 */

int	xxstrategy(), xxopen(), xxioctl();

struct devsw devsw[] = {
	{ "XX",	xxstrategy,	xxopen,		nullsys,	noioctl },
};

int	ndevs = (sizeof(devsw) / sizeof(devsw[0]));

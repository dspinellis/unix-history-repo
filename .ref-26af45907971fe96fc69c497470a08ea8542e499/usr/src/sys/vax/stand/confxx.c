/*
 * Copyright (c) 1988 Regents of the University of California.
 * All rights reserved.
 *
 * %sccs.include.redist.c%
 *
 *	@(#)confxx.c	7.6 (Berkeley) %G%
 */

int	xxstrategy(), xxopen(), xxioctl();

struct devsw devsw[] = {
	{ "XX",	xxstrategy,	xxopen,		nullsys,	noioctl },
};

int	ndevs = (sizeof(devsw) / sizeof(devsw[0]));

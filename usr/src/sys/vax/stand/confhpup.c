/*
 * Copyright (c) 1982, 1986, 1988 Regents of the University of California.
 * All rights reserved.
 *
 * %sccs.include.redist.c%
 *
 *	@(#)confhpup.c	7.8 (Berkeley) %G%
 */

#include "sys/param.h"
#include "stand/saio.h"

int	nullsys();
int	hpstrategy(), hpopen(), hpioctl();
int	upstrategy(), upopen(), upioctl();

struct devsw devsw[] = {
	{ "hp",	hpstrategy,	hpopen,		nullsys,	hpioctl },
	{ "up",	upstrategy,	upopen,		nullsys,	upioctl },
	{ 0, 0, 0, 0, 0 }
};

int	ndevs = (sizeof(devsw) / sizeof(devsw[0]) - 1);

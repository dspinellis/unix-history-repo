/*
 * Copyright (c) 1982, 1986, 1988 Regents of the University of California.
 * All rights reserved.
 *
 * Redistribution and use in source and binary forms are permitted
 * provided that this notice is preserved and that due credit is given
 * to the University of California at Berkeley. The name of the University
 * may not be used to endorse or promote products derived from this
 * software without specific prior written permission. This software
 * is provided ``as is'' without express or implied warranty.
 *
 *	@(#)confhpup.c	7.3 (Berkeley) %G%
 */

#include "param.h"
#include "fs.h"
#include "inode.h"
#include "saio.h"

int	nullsys();
int	hpstrategy(), hpopen(), hpioctl();
int	upstrategy(), upopen(), upioctl();

struct devsw devsw[] = {
	{ "hp",	hpstrategy,	hpopen,		nullsys,	hpioctl },
	{ "up",	upstrategy,	upopen,		nullsys,	upioctl },
	{ 0, 0, 0, 0, 0 }
};

int	ndevs = (sizeof(devsw) / sizeof(devsw[0]) - 1);

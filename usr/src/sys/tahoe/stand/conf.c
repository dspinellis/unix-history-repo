/*
 * Copyright (c) 1988 Regents of the University of California.
 * All rights reserved.
 *
 * %sccs.include.redist.c%
 *
 *	@(#)conf.c	1.11 (Berkeley) %G%
 */

#include "sys/param.h"
#include "sys/time.h"
#include "stand/saio.h"

extern int	nullsys(), nodev(), noioctl();

int	vdstrategy(), vdopen();
int	hdstrategy(), hdopen();
int	cystrategy(), cyopen(), cyclose();

struct devsw devsw[] = {
	{ "ud",	nodev,		nodev,	nullsys, noioctl },  /* 0 = ud */
	{ "dk",	vdstrategy,	vdopen,	nullsys, noioctl },  /* 1 = ht */
	{ "hd",	hdstrategy,	hdopen,	nullsys, noioctl },  /* 2 = hd */
#ifdef notdef
	{ "xp",	xpstrategy,	xpopen,	nullsys, noioctl },  /* 3 = xp */
#else
	{ "xp",	nodev,		nodev,	nullsys, noioctl },
#endif
	{ "cy",	cystrategy,	cyopen,	cyclose, noioctl },  /* 4 = cy */
	{ 0 }
};
int	ndevs = (sizeof(devsw)/sizeof(devsw[0]));

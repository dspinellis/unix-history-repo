/*
 * Copyright (c) 1988 Regents of the University of California.
 * All rights reserved.
 *
 * Redistribution and use in source and binary forms are permitted
 * provided that this notice is preserved and that due credit is given
 * to the University of California at Berkeley. The name of the University
 * may not be used to endorse or promote products derived from this
 * software without specific prior written permission. This software
 * is provided ``as is'' without express or implied warranty.
 *
 *	@(#)conf.c	1.6 (Berkeley) %G%
 */

#include "param.h"
#include "inode.h"
#include "fs.h"
#include "saio.h"

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

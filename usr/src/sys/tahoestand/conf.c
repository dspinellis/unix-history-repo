/*
 * Copyright (c) 1988 Regents of the University of California.
 * All rights reserved.
 *
 * Redistribution is only permitted until one year after the first shipment
 * of 4.4BSD by the Regents.  Otherwise, redistribution and use in source and
 * binary forms are permitted provided that: (1) source distributions retain
 * this entire copyright notice and comment, and (2) distributions including
 * binaries display the following acknowledgement:  This product includes
 * software developed by the University of California, Berkeley and its
 * contributors'' in the documentation or other materials provided with the
 * distribution and in all advertising materials mentioning features or use
 * of this software.  Neither the name of the University nor the names of
 * its contributors may be used to endorse or promote products derived from
 * this software without specific prior written permission.
 * THIS SOFTWARE IS PROVIDED AS IS'' AND WITHOUT ANY EXPRESS OR IMPLIED
 * WARRANTIES, INCLUDING, WITHOUT LIMITATION, THE IMPLIED WARRANTIES OF
 * MERCHANTABILITY AND FITNESS FOR A PARTICULAR PURPOSE.
 *
 *	@(#)conf.c	1.10 (Berkeley) 6/30/90
 */

#include "sys/param.h"
#include "sys/time.h"
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

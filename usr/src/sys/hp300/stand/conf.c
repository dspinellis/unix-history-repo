/*
 * Copyright (c) 1982, 1986, 1990 The Regents of the University of California.
 * All rights reserved.
 *
 * %sccs.include.redist.c%
 *
 *	@(#)conf.c	7.3 (Berkeley) %G%
 */

#include <sys/param.h>
#include "saio.h"

extern int	nullsys(), nodev(), noioctl();

#ifndef BOOT
int	ctstrategy(), ctopen(), ctclose();
#define	ctioctl	noioctl
#endif

int	rdstrategy(), rdopen();
#define	rdioctl	noioctl

int	sdstrategy(), sdopen();
#define	sdioctl	noioctl


struct devsw devsw[] = {
	{ "rd",	rdstrategy,	rdopen,	nullsys, noioctl },	/* 0 = rd */
	{ "sd",	sdstrategy,	sdopen,	nullsys, noioctl },	/* 1 = sd */
#ifndef BOOT
	{ "ct",	ctstrategy,	ctopen,	ctclose, noioctl },	/* 2 = ct */
#endif
	{ NULL },
};

int	ndevs = (sizeof(devsw)/sizeof(devsw[0]));

/*
 * Copyright (c) 1992 Regents of the University of California.
 * All rights reserved.
 *
 * This code is derived from software contributed to Berkeley by
 * Ralph Campbell.
 *
 * %sccs.include.redist.c%
 *
 *	@(#)conf.c	7.5 (Berkeley) %G%
 */

#include <stand/stand.h>

const	struct callback *callv;
int	errno;

extern int	nullsys(), nodev(), noioctl();

int	rzstrategy(), rzopen(), rzclose();
#define	rzioctl		noioctl

#ifndef BOOT
int	tzstrategy(), tzopen(), tzclose();
#endif
#define	tzioctl		noioctl


struct devsw devsw[] = {
	{ "rz",	rzstrategy,	rzopen,	rzclose,	rzioctl }, /*0*/
#ifndef BOOT
	{ "tz",	tzstrategy,	tzopen,	tzclose,	tzioctl }, /*1*/
#endif
};

int	ndevs = (sizeof(devsw)/sizeof(devsw[0]));

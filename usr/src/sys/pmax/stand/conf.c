/*
 * Copyright (c) 1992 Regents of the University of California.
 * All rights reserved.
 *
 * This code is derived from software contributed to Berkeley by
 * Ralph Campbell.
 *
 * %sccs.include.redist.c%
 *
 *	@(#)conf.c	7.4 (Berkeley) %G%
 */

#include <stand/stand.h>
#include <pmax/stand/samachdep.h>

extern int	nullsys(), nodev(), noioctl();

#if NRZ > 0
int	rzstrategy(), rzopen(), rzclose();
#else
#define	rzstrategy	nodev
#define	rzopen		nodev
#define	rzclose		nodev
#endif
#define	rzioctl		noioctl

#if NTZ > 0 && !defined(BOOT)
int	tzstrategy(), tzopen(), tzclose();
#else
#define	tzstrategy	nodev
#define	tzopen		nodev
#define	tzclose		nodev
#endif
#define	tzioctl		noioctl


struct devsw devsw[] = {
	{ "rz",	rzstrategy,	rzopen,	rzclose,	rzioctl }, /*0*/
	{ "tz",	tzstrategy,	tzopen,	tzclose,	tzioctl }, /*1*/
};

int	ndevs = (sizeof(devsw)/sizeof(devsw[0]));

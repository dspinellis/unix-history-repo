/*-
 * Copyright (c) 1990 The Regents of the University of California.
 * All rights reserved.
 *
 * This code is derived from software contributed to Berkeley by
 * William Jolitz.
 *
 * %sccs.include.redist.c%
 *
 *	@(#)conf.c	7.4 (Berkeley) %G%
 */

#include <sys/param.h>

#include <stand/saio.h>

extern int	nullsys(), nodev(), noioctl();

int	wdstrategy(), wdopen();
#define	wdioctl	noioctl

int	fdstrategy(), fdopen();
#define	fdioctl noioctl

struct devsw devsw[] = {
	{ "wd",	wdstrategy,	wdopen,	nullsys, wdioctl },	/* 0 = wd */
	{ NULL },				/* swapdev place holder */
	{ "fd",	fdstrategy,	fdopen,	nullsys, fdioctl },	/* 2 = fd */
};
int	ndevs = (sizeof(devsw)/sizeof(devsw[0]));

/*-
 * Copyright (c) 1990, 1993
 *	The Regents of the University of California.  All rights reserved.
 * (c) UNIX System Laboratories, Inc.
 * All or some portions of this file are derived from material licensed
 * to the University of California by American Telephone and Telegraph
 * Co. or Unix System Laboratories, Inc. and are reproduced herein with
 * the permission of UNIX System Laboratories, Inc.
 *
 * This code is derived from software contributed to Berkeley by
 * William Jolitz.
 *
 * %sccs.include.redist.c%
 *
 *	@(#)conf.c	8.2 (Berkeley) %G%
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

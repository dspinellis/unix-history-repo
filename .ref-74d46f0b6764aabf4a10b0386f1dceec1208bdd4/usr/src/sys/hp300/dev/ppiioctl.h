/*
 * Copyright (c) 1990 The Regents of the University of California.
 * All rights reserved.
 *
 * %sccs.include.redist.c%
 *
 *	@(#)ppiioctl.h	7.2 (Berkeley) %G%
 */

#ifndef _IOCTL_
#include <sys/ioctl.h>
#endif

struct ppiparam {
	int	burst;	/* chars to send/recv in one call */
	int	timo;	/* timeout: -1 blocking, 0 non-blocking, >0 msec */
	int	delay;	/* delay between polls (msec) */
};

#define PPI_BLOCK	-1
#define PPI_NOBLOCK	0

/* default values */
#define	PPI_BURST	1024
#define PPI_TIMO	PPI_BLOCK
#define PPI_DELAY	10

/* limits */
#define	PPI_BURST_MIN	1
#define	PPI_BURST_MAX	1024
#define PPI_DELAY_MIN	0
#define PPI_DELAY_MAX	30000

#define PPIIOCSPARAM	_IOW('P', 0x1, struct ppiparam)
#define PPIIOCGPARAM	_IOR('P', 0x2, struct ppiparam)
#define PPIIOCSSEC	_IOW('P', 0x3, int)

/*-
 * Copyright (c) 1982, 1986, 1990, 1993
 *	The Regents of the University of California.  All rights reserved.
 * (c) UNIX System Laboratories, Inc.
 * All or some portions of this file are derived from material licensed
 * to the University of California by American Telephone and Telegraph
 * Co. or Unix System Laboratories, Inc. and are reproduced herein with
 * the permission of UNIX System Laboratories, Inc.
 *
 * %sccs.include.redist.c%
 *
 *	from: @(#)kern_physio.c	8.1 (Berkeley) 6/10/93
 */

#include <sys/param.h>
#include <sys/systm.h>
#include <sys/buf.h>
#include <sys/conf.h>
#include <sys/proc.h>

physio(a1, a2, a3, a4, a5, a6)
	int (*a1)(); 
	struct buf *a2;
	dev_t a3;
	int a4;
	u_int (*a5)();
	struct uio *a6;
{

	/*
	 * Body deleted.
	 */
	return (EIO);
}

u_int
minphys(a1)
	struct buf *a1;
{

	/*
	 * Body deleted.
	 */
	return (0);
}

/*
 * Do a read on a device for a user process.
 */
rawread(dev, uio)
	dev_t dev;
	struct uio *uio;
{
	return (physio(cdevsw[major(dev)].d_strategy, (struct buf *)NULL,
	    dev, B_READ, minphys, uio));
}

/*
 * Do a write on a device for a user process.
 */
rawwrite(dev, uio)
	dev_t dev;
	struct uio *uio;
{
	return (physio(cdevsw[major(dev)].d_strategy, (struct buf *)NULL,
	    dev, B_WRITE, minphys, uio));
}

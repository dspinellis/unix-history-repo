/*-
 * Copyright (c) 1993
 *	The Regents of the University of California.  All rights reserved.
 *
 * %sccs.include.redist.c%
 *
 *	@(#)dev.c	8.1 (Berkeley) %G%
 */

#include <stand/stand.h>

/* ARGSUSED */
nodev(io)
	struct iob *io;
{
	return (ENXIO);
}

/* ARGSUSED */
noioctl(io, cmd, arg)
	struct iob *io;
	int cmd;
	caddr_t arg;
{
	return (EINVAL);
}

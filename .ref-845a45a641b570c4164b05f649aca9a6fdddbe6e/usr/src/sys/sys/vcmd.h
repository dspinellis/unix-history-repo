/*-
 * Copyright (c) 1982, 1986, 1993
 *	The Regents of the University of California.  All rights reserved.
 *
 * %sccs.include.redist.c%
 *
 *	@(#)vcmd.h	8.1 (Berkeley) %G%
 */

#include <sys/ioctl.h>

#define	VPRINT		0100
#define	VPLOT		0200
#define	VPRINTPLOT	0400

#define	VGETSTATE	_IOR('v', 0, int)
#define	VSETSTATE	_IOW('v', 1, int)

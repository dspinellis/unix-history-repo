/*
 * Copyright (c) 1989 University of Utah.
 * Copyright (c) 1990 The Regents of the University of California.
 * All rights reserved.
 *
 * This code is derived from software contributed to Berkeley by
 * the Systems Programming Group of the University of Utah Computer
 * Science Department.
 *
 * %sccs.include.redist.c%
 *
 * from: Utah $Hdr: clockioctl.h 1.1 90/07/09$
 *
 *	@(#)clockioctl.h	7.2 (Berkeley) %G%
 */

#define	CLOCKMAP	_IOWR('C', 1, int)
#define	CLOCKUNMAP	_IOW('C', 2, int)
#define	CLOCKGETRES	_IOR('C', 3, int)

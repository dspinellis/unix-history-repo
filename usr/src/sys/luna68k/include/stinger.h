/*
 * Copyright (c) 1992 OMRON Corporation.
 * Copyright (c) 1992 The Regents of the University of California.
 * All rights reserved.
 *
 * This code is derived from software contributed to Berkeley by
 * OMRON Corporation.
 *
 * %sccs.include.redist.c%
 *
 *	@(#)stinger.h	7.3 (Berkeley) %G%
 */

/*
 * stinger.h -- Stinger Kernel Interface Definitions
 *   remade by A.Fujita, JAN-12-1993
 */

struct KernInter {
	caddr_t	maxaddr;
	u_int	dipsw;
	int	plane;
};

extern struct KernInter KernInter;

#define	KIFF_DIPSW_NOBM		0x0002		/* not use bitmap display as console */

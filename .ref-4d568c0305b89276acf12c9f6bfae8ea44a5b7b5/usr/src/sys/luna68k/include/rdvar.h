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
 *	@(#)rdvar.h	7.1 (Berkeley) %G%
 */

/*
 * rdvar.h -- ??? ( This file should be removed )
 * by A.Fujita, MAR-14-1992
 */

struct rd_partition {
	u_long  p_size;
	u_char *p_addr;
	int	p_fstype;
};

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
 *	@(#)memory.h	7.1 (Berkeley) %G%
 */

/*
 * memory.h -- ??? ( This file should be removed )
 * by A.Fujita, MAR-14-1992
 */


struct prgmem {
	u_long pg_start;
	u_long pg_end;
	u_long pg_stack;
};

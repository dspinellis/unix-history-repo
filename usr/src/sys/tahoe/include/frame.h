/*-
 * Copyright (c) 1991 The Regents of the University of California.
 * All rights reserved.
 *
 * This code is derived from software contributed to Berkeley by
 * Computer Consoles Inc.
 *
 * %sccs.include.redist.c%
 *
 *	@(#)frame.h	7.2 (Berkeley) %G%
 */

/*
 * Definition of the tahoe call frame.
 */
struct frame {
	int	fr_savpc;		/* saved program counter */
	u_int	fr_mask:16,		/* register save mask */
		fr_removed:16;		/* 4*number of arguments + 4 */
	int	fr_savfp;		/* saved frame pointer */
};

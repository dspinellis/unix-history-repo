/*
 * Copyright (c) 1991 Regents of the University of California.
 * All rights reserved.
 *
 * %sccs.include.redist.c%
 *
 *	@(#)proc.h	7.2 (Berkeley) %G%
 */

/*
 * Machine-dependent part of the proc structure for hp300.
 */
struct mdproc {
	int	md_flags;		/* machine-dependent flags */
	int	*md_regs;		/* registers on current frame */
};

/* md_flags */
#define	MDP_AST		0x0001	/* async trap pending */

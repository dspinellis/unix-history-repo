/*
 * Copyright (c) 1992 The Regents of the University of California.
 * All rights reserved.
 *
 * This software was developed by the Computer Systems Engineering group
 * at Lawrence Berkeley Laboratory under DARPA contract BG 91-66 and
 * contributed to Berkeley.
 *
 * %sccs.include.redist.c%
 *
 *	@(#)proc.h	7.1 (Berkeley) %G%
 *
 * from: $Header: proc.h,v 1.5 92/06/17 06:10:23 torek Exp $ (LBL)
 */

/*
 * Machine-dependent part of the proc structure for SPARC.
 */
struct mdproc {
	struct	trapframe *md_tf;	/* trap/syscall registers */
	struct	fpstate *md_fpstate;	/* fpu state, if any; always resident */
};

/*
 * Copyright (c) 1992 The Regents of the University of California.
 * All rights reserved.
 *
 * This software was developed by the Computer Systems Engineering group
 * at Lawrence Berkeley Laboratory under DARPA contract BG 91-66 and
 * contributed to Berkeley.
 *
 * All advertising materials mentioning features or use of this software
 * must display the following acknowledgement:
 *	This product includes software developed by the University of
 *	California, Lawrence Berkeley Laboratory.
 *
 * %sccs.include.redist.c%
 *
 *	@(#)proc.h	7.3 (Berkeley) %G%
 *
 * from: $Header: proc.h,v 1.6 92/11/26 02:04:41 torek Exp $ (LBL)
 */

/*
 * Machine-dependent part of the proc structure for SPARC.
 */
struct mdproc {
	struct	trapframe *md_tf;	/* trap/syscall registers */
	struct	fpstate *md_fpstate;	/* fpu state, if any; always resident */
};

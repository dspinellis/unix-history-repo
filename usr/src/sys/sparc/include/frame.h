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
 *	@(#)frame.h	7.1 (Berkeley) %G%
 *
 * from: $Header: frame.h,v 1.4 92/06/17 06:10:17 torek Exp $
 */

/*
 * Sparc stack frame format.
 *
 * Note that the contents of each stack frame may be held only in
 * machine register windows.  In order to get an accurate picture
 * of the frame, you must first force the kernel to write any such
 * windows to the stack.
 */
struct frame {
	int	fr_local[8];	/* space to save locals (%l0..%l7) */
	int	fr_arg[6];	/* space to save arguments (%i0..%i5) */
	struct	frame *fr_fp;	/* space to save frame pointer (%i6) */
	int	fr_pc;		/* space to save return pc (%i7) */
	/*
	 * SunOS reserves another 8 words here; this is pointless
	 * but we do it for compatibility.
	 */
	int	fr_xxx;		/* `structure return pointer' (unused) */
	int	fr_argd[6];	/* `arg dump area' (lunacy) */
	int	fr_argx[1];	/* arg extension (args 7..n; variable size) */
};

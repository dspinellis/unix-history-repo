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
 *	California, Lawrence Berkeley Laboratories.
 *
 * %sccs.include.redist.c%
 *
 *	@(#)reg.h	7.3 (Berkeley) %G%
 *
 * from: $Header: reg.h,v 1.7 92/06/17 06:10:26 torek Exp $
 */

#ifndef _MACHINE_REG_H_
#define	_MACHINE_REG_H_

/*
 * Registers passed to trap/syscall/etc.
 * This structure is known to occupy exactly 80 bytes (see locore.s).
 * Note, tf_global[0] is not actually written (since g0 is always 0).
 * (The slot tf_global[0] is used to send a copy of %wim to kernel gdb.
 * This is known as `cheating'.)
 */
struct trapframe {
	int	tf_psr;		/* psr */
	int	tf_pc;		/* return pc */
	int	tf_npc;		/* return npc */
	int	tf_y;		/* %y register */
	int	tf_global[8];	/* global registers in trap's caller */
	int	tf_out[8];	/* output registers in trap's caller */
};

/*
 * Register windows.  Each stack pointer (%o6 aka %sp) in each window
 * must ALWAYS point to some place at which it is safe to scribble on
 * 64 bytes.  (If not, your process gets mangled.)  Furthermore, each
 * stack pointer should be aligned on an 8-byte boundary (the kernel
 * as currently coded allows arbitrary alignment, but with a hefty
 * performance penalty).
 */
struct rwindow {
	int	rw_local[8];		/* %l0..%l7 */
	int	rw_in[8];		/* %i0..%i7 */
};

#include <machine/fsr.h>

/*
 * FP coprocessor registers.
 *
 * FP_QSIZE is the maximum coprocessor instruction queue depth
 * of any implementation on which the kernel will run.  David Hough:
 * ``I'd suggest allowing 16 ... allowing an indeterminate variable
 * size would be even better''.  Of course, we cannot do that; we
 * need to malloc these.
 */
#define	FP_QSIZE	16

struct fp_qentry {
	int	*fq_addr;		/* the instruction's address */
	int	fq_instr;		/* the instruction itself */
};
struct fpstate {
	u_int	fs_regs[32];		/* our view is 32 32-bit registers */
	int	fs_fsr;			/* %fsr */
	int	fs_qsize;		/* actual queue depth */
	struct	fp_qentry fs_queue[FP_QSIZE];	/* queue contents */
};

#endif /* _MACHINE_REG_H_ */

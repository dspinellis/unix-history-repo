/*-
 * Copyright (c) 1985 The Regents of the University of California.
 * All rights reserved.
 *
 * This code is derived from software contributed to Berkeley by
 * Computer Consoles Inc.
 *
 * %sccs.include.redist.c%
 *
 *	@(#)Kfp_mvtoacc.s	7.1 (Berkeley) %G%
 */

#include "../math/fp.h"
#include "../tahoe/SYS.h"

/*
 * mvtofacc(value, acc_addr)
 *
 * move value to floating point accumulator
 */
ENTRY(mvtofacc, 0)
	movl	4(fp),*12(fp)
	ret

/*
 * mvtodacc(value_hi, value_lo, acc_addr)
 *
 * move value to double precision accumulator
 */
ENTRY(mvtodacc, 0)
	movl	12(fp),r0	# address of accumulator
	movl	4(fp),(r0)	# most significant longword
	movl	8(fp),4(r0)	# least significant longword
	ret

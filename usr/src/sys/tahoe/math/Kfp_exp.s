/*-
 * Copyright (c) 1985 The Regents of the University of California.
 * All rights reserved.
 *
 * This code is derived from software contributed to Berkeley by
 * Computer Consoles Inc.
 *
 * %sccs.include.redist.c%
 *
 *	@(#)Kfp_exp.s	7.1 (Berkeley) %G%
 */

#include "../math/fp.h"
#include "../math/Kfp.h"
#include "../tahoe/SYS.h"

ENTRY(Kfpover, 0)
	movl	$HUGE0,r0
	movl	$HUGE1,r1
	ret

ENTRY(Kfpunder, 0)
	clrl	r0
	clrl	r1
	ret

ENTRY(Kfpzdiv, 0)
	divl2	$0,r1		# force division by zero.
	ret

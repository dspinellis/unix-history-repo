/*
 * Copyright (c) 1986 Regents of the University of California.
 * All rights reserved.
 *
 * This code is derived from software contributed to Berkeley by
 * Computer Consoles Inc.
 *
 * %sccs.include.redist.c%
 */

#if defined(SYSLIBC_SCCS) && !defined(lint)
	.asciz "@(#)sfp_exp.s	1.3 (Berkeley) %G%"
#endif /* SYSLIBC_SCCS and not lint */

#include <tahoemath/fp.h>
#include "DEFS.h"

/*
 * Reserved floating point operand.
 */
ASENTRY(sfpresop, 0)
	movl	$0xaaaaaaaa,r0
	clrl	r1
	ret

/*
 * Floating point overflow.
 */
ASENTRY(sfpover, 0)
	movl	$HUGE0,r0
	clrl	r1
	ret

/*
 * Floating point underflow.
 */
ASENTRY(sfpunder, 0)
	clrl	r0
	clrl	r1
	ret

/*
 * Floating point division by zero.
 */
ASENTRY(sfpzdiv, 0)
	divl2	$0,r0		# force division by zero.
	ret

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
	.asciz "@(#)cmpf.s	1.3 (Berkeley) %G%"
#endif /* SYSLIBC_SCCS and not lint */

/*
 * cmpf(f1, f2)
 *	float f1, f2;
 * return -1, 0, 1 as f1 <, ==, > f2
 */
#include "DEFS.h"

XENTRY(cmpf, 0)
	cmpl	4(fp),12(fp)
	jneq	1f
	clrl	r0
	ret
1:
	movl	4(fp),r0
	jgeq	1f
	xorl2	$0x80000000,r0
	mnegl	r0,r0
1:
	movl	12(fp),r1
	jgeq	1f
	xorl2	$0x80000000,r1
	mnegl	r1,r1
1:
	cmpl	r0,r1
	jleq	1f
	movl	$1,r0
	ret
1:
	mnegl	$1,r0
	ret

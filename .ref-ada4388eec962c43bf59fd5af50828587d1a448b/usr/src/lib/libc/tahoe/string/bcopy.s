/*
 * Copyright (c) 1988, 1993
 *	The Regents of the University of California.  All rights reserved.
 *
 * %sccs.include.redist.c%
 *
 * This code is derived from software contributed to Berkeley by
 * Computer Consoles Inc.
 */

#if defined(LIBC_SCCS) && !defined(lint)
	.asciz "@(#)bcopy.s	8.1 (Berkeley) %G%"
#endif /* LIBC_SCCS and not lint */

/* bcopy(from, to, size) */
#include "DEFS.h"

ENTRY(bcopy, 0)
	movl	4(fp),r0
	movl	8(fp),r1
	movl	12(fp),r2
	cmpl	r0,r1
	bgtru	1f		# normal forward case
	beql	2f		# equal, nothing to do
	addl2	r2,r0		# may be overlapping
	cmpl	r0,r1
	bgtru	3f
	subl2	r2,r0		# normal forward case
1:
	movblk
2:
	ret
3:
	addl2	r2,r1		# overlapping, must do backwards
	subl3	r0,r1,r3
	movl	r2,r4
	jbr	5f
4:
	subl2	r3,r0
	subl2	r3,r1
	movl	r3,r2
	movblk
	subl2	r3,r0
	subl2	r3,r1
	subl2	r3,r4
5:
	cmpl	r4,r3
	jgtr	4b
	movl	r4,r2
	subl2	r2,r0
	subl2	r2,r1
	movblk
	ret

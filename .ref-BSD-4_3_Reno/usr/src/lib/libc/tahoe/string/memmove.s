/*
 * Copyright (c) 1988 Regents of the University of California.
 * All rights reserved.
 *
 * Redistribution and use in source and binary forms are permitted provided
 * that: (1) source distributions retain this entire copyright notice and
 * comment, and (2) distributions including binaries display the following
 * acknowledgement:  ``This product includes software developed by the
 * University of California, Berkeley and its contributors'' in the
 * documentation or other materials provided with the distribution and in
 * all advertising materials mentioning features or use of this software.
 * Neither the name of the University nor the names of its contributors may
 * be used to endorse or promote products derived from this software without
 * specific prior written permission.
 * THIS SOFTWARE IS PROVIDED ``AS IS'' AND WITHOUT ANY EXPRESS OR IMPLIED
 * WARRANTIES, INCLUDING, WITHOUT LIMITATION, THE IMPLIED WARRANTIES OF
 * MERCHANTABILITY AND FITNESS FOR A PARTICULAR PURPOSE.
 *
 * This code is derived from software contributed to Berkeley by
 * Computer Consoles Inc.
 */

#if defined(LIBC_SCCS) && !defined(lint)
	.asciz "@(#)memmove.s	5.2 (Berkeley) 6/1/90"
#endif /* LIBC_SCCS and not lint */

/* void *memmove(to, from, size) */
#include "DEFS.h"

ENTRY(memmove, 0)
	movl	4(fp),r1
	movl	8(fp),r0
	movl	12(fp),r2
	cmpl	r0,r1
	bgtru	1f		/* normal forward case */
	beql	2f		/* equal, nothing to do */
	addl2	r2,r0		/* may be overlapping */
	cmpl	r0,r1
	bgtru	3f
	subl2	r2,r0		/* normal forward case */
1:
	movblk
2:
	movl	4(fp),r0	/* this can probably be optimised */
	ret
3:
	addl2	r2,r1		/* overlapping, must do backwards */
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
	movl	4(fp),r0
	ret

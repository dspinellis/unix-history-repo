/*-
 * Copyright (c) 1985 The Regents of the University of California.
 * All rights reserved.
 *
 * This code is derived from software contributed to Berkeley by
 * Computer Consoles Inc.
 *
 * %sccs.include.redist.c%
 *
 *	@(#)Kcmpf.s	7.1 (Berkeley) %G%
 */

#include "../tahoe/SYS.h"

/*
 * cmpf(o1, o2)
 *	register o1, o2;
 *{
 *	if(o1 < 0) {
 *		o1 ^= 0x80000000;
 *		o1 = -o1;
 *	}
 *	if(o2 < 0) {
 *		o2 ^= 0x80000000;
 *		o2 = -o2;
 *	}
 *	if(o1 != o2)
 *		return(o1>o2 ? 1 : -1);
 *	return(0);
 *}
 */
	.text
ENTRY(Kcmpf, 0)
	movl	4(fp),r12
	jgeq	1f
	xorl2	$0x80000000,r12
	mnegl	r12,r12
1:
	movl	12(fp),r11
	jgeq	1f
	xorl2	$0x80000000,r11
	mnegl	r11,r11
1:
	cmpl	r12,r11
	jneq	1f
	clr	r0; ret
1:
	jleq	1f
	movl	$1,r0; ret
1:
	mnegl	$1,r0; ret

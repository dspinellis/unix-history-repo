/*-
 * Copyright (c) 1985 The Regents of the University of California.
 * All rights reserved.
 *
 * This code is derived from software contributed to Berkeley by
 * Computer Consoles Inc.
 *
 * %sccs.include.redist.c%
 *
 *	@(#)Kcmpd.s	7.1 (Berkeley) %G%
 */

#include "../tahoe/SYS.h"

/*
 * cmpd(hi1, lo1, hi2, lo2)
 *	register hi1, hi2;
 *	register unsigned lo1, lo2;
 *{
 *	if(hi1 < 0) {
 *		hi1 ^= 0x80000000;
 *		if(lo1) {
 *			lo1 = -lo1;
 *			hi1 = ~hi1;
 *		} else
 *			hi1 = -hi1;
 *	}
 *	if(hi2 < 0) {
 *		hi2 ^= 0x80000000;
 *		if(lo2) {
 *			lo2 = -lo2;
 *			hi2 = ~hi2;
 *		} else
 *			hi2 = -hi2;
 *	}
 *	if(hi1 != hi2)
 *		return(hi1>hi2 ? 1 : -1);
 *	if(lo1 != lo2)
 *		return(lo1>lo2 ? 1 : -1);
 *	return(0);
 *}
 */
	.text
ENTRY(Kcmpd, 0)
	movl	8(fp),r3
	movl	12(fp),r4
	movl	16(fp),r2
	movl	4(fp),r5
	jgeq	1f
	xorl2	$0x80000000,r5
	tstl	r3
	jeql	2f
	mnegl	r3,r3
	mcoml	r5,r5
	jbr	1f
2:
	mnegl	r5,r5
1:
	tstl	r4
	jgeq	1f
	xorl2	$0x80000000,r4
	tstl	r2
	jeql	2f
	mnegl	r2,r2
	mcoml	r4,r4
	jbr	1f
2:
	mnegl	r4,r4
1:
	cmpl	r5,r4
	jeql	1f
	jleq	2f
	movl	$1,r0
	ret
2:
	mnegl	$1,r0
	ret
1:
	cmpl	r3,r2
	jeql	1f
	jlequ	2f
	movl	$1,r0
	ret
2:
	mnegl	$1,r0
	ret
1:
	clrl	r0
	ret

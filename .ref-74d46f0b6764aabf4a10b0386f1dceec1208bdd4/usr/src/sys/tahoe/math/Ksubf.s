/*-
 * Copyright (c) 1985 The Regents of the University of California.
 * All rights reserved.
 *
 * This code is derived from software contributed to Berkeley by
 * Computer Consoles Inc.
 *
 * %sccs.include.redist.c%
 *
 *	@(#)Ksubf.s	7.1 (Berkeley) %G%
 */

#include "../tahoe/SYS.h"

/*
 * _Ksubf(acc_most,acc_least,op_most,op_least,hfs)
 *
 * subf(d1,d2)
 * {
 * 	return(d1+(-d2));
 * }
 */
	.text
ENTRY(Ksubf, 0)
	tstl	4(fp)	# handle (a-b) where a and/or b = 0.0
	jneq	next
	movl	$0,r1
	movl	12(fp),r0
	lnd	r0
	std	r0
	ret
next:
	tstl	12(fp)
	jneq	doit
	movl	$0,r1
	movl	4(fp),r0
	ret
doit:
	lnd	12(fp)	# note: -(0) = 0 by definition!
	pushl	20(fp)	# hfs
	pushd	
	pushl	8(fp)
	pushl	4(fp)	# acc
	callf	$24,_Kaddf
	ret

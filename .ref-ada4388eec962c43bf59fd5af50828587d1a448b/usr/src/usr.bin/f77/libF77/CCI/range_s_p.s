/*-
 * Copyright (c) 1991 The Regents of the University of California.
 * All rights reserved.
 *
 * This code is derived from software contributed to Berkeley by
 * Computer Consoles Inc.
 *
 * %sccs.include.proprietary.c%
 */

#ifndef lint
	.asciz "@(#)range_s_p.s	5.2 (Berkeley) %G%"
#endif /* not lint */

.data
.text
LL0:.align	1
.globl	_flmax_
.set	MASK__1,0x0
.data
.text
_flmax_:.word	MASK__1
	.data
LLLL1:	.align	2
	.long	0
	.text
	pushl	$LLLL1
	callf	$8,mcount
movl	$2147483647,r0
ret#1

.align	1
.globl	_dflmax_
.set	MASK__2,0x0
.data
.text
_dflmax_:.word	MASK__2
	.data
LLLL2:	.align	2
	.long	0
	.text
	pushl	$LLLL2
	callf	$8,mcount
movl	$2147483647,r0
movl	$0xffffffff,r1
ret#2

.align	1
.globl	_flmin_
.set	MASK__3,0x0
.data
.text
_flmin_:.word	MASK__3
	.data
LLLL3:	.align	2
	.long	0
	.text
	pushl	$LLLL3
	callf	$8,mcount
movl	$8388608,r0
ret#1

.align	1
.globl	_dflmin_
.set	MASK__4,0x0
.data
.text
_dflmin_:.word	MASK__4
	.data
LLLL4:	.align	2
	.long	0
	.text
	pushl	$LLLL4
	callf	$8,mcount
movl	$8388608,r0
clrl	r1
ret#2

.align	1
.globl	_inmax_
.set	MASK__5,0x0
.data
.text
_inmax_:.word	MASK__5
	.data
LLLL5:	.align	2
	.long	0
	.text
	pushl	$LLLL5
	callf	$8,mcount
movl	$2147483647,r0
ret#1

.align	1
.globl	_ffrac_
.set	MASK__6,0x0
.data
.text
_ffrac_:.word	MASK__6
	.data
LLLL6:	.align	2
	.long	0
	.text
	pushl	$LLLL6
	callf	$8,mcount
movl	$889192448,r0
ret#1

.align	1
.globl	_dffrac_
.set	MASK__7,0x0
.data
.text
_dffrac_:.word	MASK__7
	.data
LLLL7:	.align	2
	.long	0
	.text
	pushl	$LLLL7
	callf	$8,mcount
movl	$620756992,r0
clrl	r1
ret#2


# @(#)abs.s	4.1 (Berkeley) 12/21/80
# abs - int absolute value.
# fabs - floating abs

.globl	_abs
	.align	1
_abs:
	.word	0x0000
	movl	4(ap),r0
	bgeq	absl
	mnegl	r0,r0
absl:
	ret

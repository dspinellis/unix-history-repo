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

.globl	_fabs
	.align	1
_fabs:
	.word	0x0000
	movd	4(ap),r0
	bgeq	fabsl
	mnegd	r0,r0
fabsl:
	ret

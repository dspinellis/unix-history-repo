.globl	_fabs
	.align	1
_fabs:
	.word	0x0000
	movd	4(ap),r0
	bgeq	fabsl
	mnegd	r0,r0
fabsl:
	ret

# C library -- abort

.globl	_abort

	.align	1
_abort:
	.word	0x0000
	halt
	clrl	r0
	ret

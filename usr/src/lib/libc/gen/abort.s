# @(#)abort.s	4.1 (Berkeley) 12/21/80
# C library -- abort

.globl	_abort

	.align	1
_abort:
	.word	0x0000
	halt
	clrl	r0
	ret

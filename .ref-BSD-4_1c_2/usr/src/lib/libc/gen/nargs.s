# @(#)nargs.s	4.1 (Berkeley) 12/21/80
# C library -- nargs


.globl	_nargs

_nargs:
	.word	0x0000
	movzbl	*8(fp),r0	# 8(fp) is old ap
	ret

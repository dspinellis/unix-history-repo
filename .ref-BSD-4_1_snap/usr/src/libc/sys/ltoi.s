# @(#)ltoi.s	4.1 (Berkeley) 12/21/80
#
# ltoi(long) returns the long as an int.

.globl	_ltoi
_ltoi:
	.word	0x0000
	movl	4(ap),r0
	ret

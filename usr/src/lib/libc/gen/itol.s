# @(#)itol.s	4.1 (Berkeley) 12/21/80
#
# Convert pair of integers to a long
#

.globl	_itol

_itol:
	.word	0x0000
	ashl	$16,4(ap),r0
	bicl3	$0xffff0000,8(ap),r1
	addl2	r1,r0
	ret

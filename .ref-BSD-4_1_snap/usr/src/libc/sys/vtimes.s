# @(#)vtimes.s	4.1 (Berkeley) 12/21/80
# C library -- times

	.set	vtimes,64+43
.globl	_vtimes

_vtimes:
	.word	0x0000
	chmk	$vtimes
	ret

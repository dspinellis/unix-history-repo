# C library -- times

	.set	vtimes,64+43
.globl	_vtimes

_vtimes:
	.word	0x0000
	chmk	$vtimes
	ret
